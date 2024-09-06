import logging
import itertools
from pprint import pprint
import asyncio
import socket
from dataclasses import dataclass
from typing import List
import struct

import cocotb
from cocotb.utils import get_sim_time
from cocotb.clock import Clock
from cocotb.triggers import Timer, RisingEdge, Event
from cocotb.types import Logic, LogicArray  #, ModifiableObject
from cocotb.handle import ModifiableObject, SimHandleBase
from cocotb.queue import Queue

from cocotbext.wishbone.driver import WishboneMaster, WBOp

logger = logging.getLogger('Wishbone test')
logger.setLevel(logging.DEBUG)


def bytes_to_ints(dat: bytes, fmt:str='>I') -> List[int]:
    # Unpack byte into 4 byte long words (int format) (with default fmt)
    return [ x[0] for x in struct.iter_unpack(fmt, dat) ]


@dataclass
class WishboneMasterPort:
    '''Master port of the HDL (data from HW to tb)

    `_i` is input to cocotb
    `_o` is output to cocotb
    '''

    # Out signals should not be modifiable, not sure what handle to use
    # otherwise. Cocotb sets the type of these signals to modifiable anyways.

    # Common signals
    dat_o:      ModifiableObject
    dat_i:      ModifiableObject

    # Master signals
    adr_o:      ModifiableObject
    sel_o:      ModifiableObject
    stb_o:      ModifiableObject
    cyc_o:      ModifiableObject
    we_o:       ModifiableObject

    ack_i:      ModifiableObject
    stall_i:    ModifiableObject|None
    err_i:      ModifiableObject|None
    rty_i:      ModifiableObject|None


def init_wishbone_masters(wbs: List[WishboneMasterPort]):
    for wb in wbs:
        wb.dat_i.value = 0
        wb.ack_i.value = 0
        if wb.stall_i:
            wb.stall_i.value = 0
        if wb.err_i:
            wb.err_i.value = 0
        if wb.rty_i:
            wb.rty_i.value = 0


class WishboneMasterMonitor:
    '''Wishbone master port monitor

    This monitor uses an always on task to make sure that packets are also
    received if a test is not currently receiving.

    Check every clock cycle if data is valid. Store valid data in list. Once
    stream is finished (signalled with an event) combine data in the list into
    one 'packet' that is awaited from recv()
    '''
    def __init__(self, clk: SimHandleBase, port: WishboneMasterPort):
        self.clk = clk
        self.port = port
        self.packet_queue = Queue()
        self._words = []
        self._words_ready = Event()

        self._handle = None

    async def _word_capture_task(self):
        tx = self.port

        while True:
            await RisingEdge(self.clk)

            stalled = tx.stall_i and tx.stall_i.value

            if tx.cyc_o.value:
                # TX running (cyc high)

                if tx.stb_o.value and not stalled:
                    # This dev selected and line not stalled
                    tx.ack_i.value = 1
                    word = tx.dat_o.value
                    self._words.append(word)
                    logger.debug(f'Read monitor word: {hex(int(word))}')
                else:
                    # Device not selected, or stalled
                    tx.ack_i.value = 0
            else:
                # TX finished (cyc low)
                tx.ack_i.value = 0

                if self._words:
                    self._words_ready.set()

    async def _task(self):
        word_task = cocotb.start_soon(self._word_capture_task())

        try:
            while True:
                await self._words_ready.wait()

                # Format words into a single byte string
                dat = b''.join([
                    int(x).to_bytes(4) for x in self._words
                ])
                self._words.clear()
                logger.debug(f'Monitor datastream: {dat.hex()}')
                await self.packet_queue.put(dat)

                self._words_ready.clear()
        except asyncio.CancelledError:
            word_task.cancel()

    async def recv(self) -> bytes:
        if not self._handle and self.packet_queue.empty():
            raise RuntimeError('Monitor is not running and no packets left')
        return await self.packet_queue.get()

    def start(self):
        if self._handle:
            raise RuntimeError('Monitor already running')
        self._handle = cocotb.start_soon(self._task())

    def stop(self):
        if not self._handle:
            raise RuntimeError('Monitor is not running')
        self._handle.cancel()


class EtherboneDUT:
    def __init__(self, dut, period=(10, 'ns')):
        self.dut = dut

        self.clock = Clock(dut.clk, period[0], units=period[1])
        cocotb.start_soon(self.clock.start())

        self.rx = WishboneMaster(dut, "rx", dut.clk,
                                 width=32,
                                 timeout=20,
                                 signals_dict={"cyc":  "cyc_i",
                                             "stb":  "stb_i",
                                             "we":   "we_i",
                                             "adr":  "adr_i",
                                             "datwr":"dat_i",
                                             "datrd":"dat_o",
                                             "ack":  "ack_o" })

        self.cfg = WishboneMaster(dut, "cfg", dut.clk,
                                  width=32,
                                  timeout=20,
                                  signals_dict={"cyc":  "cyc_i",
                                              "stb":  "stb_i",
                                              "we":   "we_i",
                                              "adr":  "adr_i",
                                              "datwr":"dat_i",
                                              "datrd":"dat_o",
                                              "ack":  "ack_o" })

        self.tx_port = WishboneMasterPort(
            dut.tx_dat_o,
            dut.tx_dat_i,
            dut.tx_adr_o,
            dut.tx_sel_o,
            dut.tx_stb_o,
            dut.tx_cyc_o,
            dut.tx_we_o,
            dut.tx_ack_i,
            dut.tx_stall_i,
            dut.tx_err_i,
            dut.tx_rty_i,
        )
        init_wishbone_masters([self.tx_port])

        self.tx = WishboneMasterMonitor(dut.clk, self.tx_port)
        self.tx.start()

        dut.rst_n.value = 0

    async def reset(self, duration=(100, 'ns')):
        self.dut.rst_n.value = 0
        init_wishbone_masters([self.tx_port])
        await Timer(*duration)
        self.dut.rst_n.value = 1
        await Timer(*duration)

    # Send bytes to the RX wishbone port
    def send(self, data: bytes):
        return self.rx.send_cycle([
            WBOp(0, x) for x in bytes_to_ints(data)
        ])

    # Receive data from the TX wishbone port
    # This function uses a WishboneMasterMonitor, this function returns any
    # block that has already been finished or waits for the next block
    async def recv(self) -> bytes:
        return await self.tx.recv()


@cocotb.test(skip=True)
# @cocotb.test()
async def test_etherbone(dut):
    edge = RisingEdge(dut.clk)

    wb_rx = WishboneMaster(dut, "rx", dut.clk,
                         width=32,   # size of data bus
                         timeout=10, # in clock cycle number
                         signals_dict={"cyc":  "cyc_i",
                                     "stb":  "stb_i",
                                     "we":   "we_i",
                                     "adr":  "adr_i",
                                     "datwr":"dat_i",
                                     "datrd":"dat_o",
                                     "ack":  "ack_o" })

    tx = WishboneMasterPort(
        dut.tx_dat_o,
        dut.tx_dat_i,
        dut.tx_adr_o,
        dut.tx_sel_o,
        dut.tx_stb_o,
        dut.tx_cyc_o,
        dut.tx_we_o,
        dut.tx_ack_i,
        dut.tx_stall_i,
        dut.tx_err_i,
        dut.tx_rty_i,
    )

    init_wishbone_masters([tx])


    await init_test(dut)

    await Timer(100, 'ns')

    PROBE_STR = '4e6f11ff00000086'
    # PROBE_STR = '45000024f003400040114cc37f0000017f000001c0ed11110010fe234e6f11ff00000086'
    # PROBE = bytestr_to_bytes('c0ed11110010fe234e6f11ff00000086')
    # await wbs.send_cycle([WBOp(0b10, bytes.fromhex(PROBE_STR))])

    LONG_STR = '4e6f1044a00f0008000080020000800000008004000080080000800c0000801000008014000080180000801ce80f00010000800300000004'

    await Timer(100, 'ns')

    # return

    # await wb_rx.send_cycle([
    #     WBOp(0b10, int.from_bytes(x)) for x in bytestr_to_bytes(PROBE_STR, 8)
    # ])

    dut.tx_ack_i.value = 1

    await cocotb.start(wb_rx.send_cycle([
        WBOp(0b00, x) for x in str_to_ints(LONG_STR, 8)
    ]))

    # for _ in range(4):
    #     transaction = await wb_tx.wait_for_recv()
    #     print(f"Received: {pprint(hex(transaction[0].datwr))}")

    # buf = []
    # for _ in range(20):
    #     await edge
    #     if dut.tx_stb.value and dut.tx_cyc.value:
    #         buf.append(int(dut.tx_datwr))
    #         dut.tx_ack.value = 1
    #     else:
    #         dut.tx_ack.value = 0


    await Timer(2000, 'ns')


# class UDPServerProtocol(asyncio.DatagramProtocol):
#     def __init__(self, queue):
#         print('protocol init')
#         self.queue = queue
#         self.transport = None
#
#     def connection_made(self, transport):
#         self.transport = transport
#
#     def datagram_received(self, data, addr):
#         logger.info(f'MSG received: {data}')
#         self.queue.put_nowait((data,addr))




@cocotb.test(skip=False)
async def test_socket(dut):
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('0.0.0.0', 0x1111))

    eb_dut = EtherboneDUT(dut)
    await eb_dut.reset()

    try:
        while True:
            data, addr = sock.recvfrom(1024)
            print('udp got', data.hex())

            cocotb.start_soon(eb_dut.send(data))
            resp = await eb_dut.recv()

            sock.sendto(resp, addr)
            print('sent', resp)

            # Wait some time so that the waveform is more readable
            await Timer(200, 'ns')
    except KeyboardInterrupt:
        pass
    finally:
        sock.close()



'''
Probe packet from eb-ls
00000000  4e 6f 11 ff 00 00 00 86  4e 6f 11 ff 00 00 00 86  |No......No......|
0x11 = 0b00010001 
            |   ^ probe flag (PF)
            ^ Version
0xff  -- AddrSz and PortSz (just ones?)
the rest (0x00000086) is 4-byte probe identifier (?)


Wait, I need IP/UDP header as well
With header:
45000024f003400040114cc37f0000017f000001 c0ed11110010fe23 4e6f11ff00000086
    |                   |       |        ^ header        ^ EB probe packet
    |                   |       |                ^ UDP length
    ^ IP length         ^ src   ^ dst

    - UDP header length field is length of header + data (0x0010 = 16 bytes)
'''
