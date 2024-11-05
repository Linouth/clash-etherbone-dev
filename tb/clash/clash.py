import logging
from logging import Logger
import itertools
from pprint import pprint
import asyncio
import socket
from dataclasses import dataclass
from typing import List
import struct
from collections import OrderedDict

import cocotb
from cocotb.utils import get_sim_time
from cocotb.clock import Clock
from cocotb.triggers import Timer, RisingEdge, Event
from cocotb.types import Logic, LogicArray  #, ModifiableObject
from cocotb.handle import ModifiableObject, SimHandleBase
from cocotb.queue import Queue
from cocotb.regression import TestFactory

from cocotbext.wishbone.driver import WishboneMaster, WBOp

''' Improvements
- runner instead of Makefile
- Proper test for each set of TXs (instead of all chunked together)
- Decoder and encoder for EB packets (records mostly)
'''

logger = logging.getLogger('Etherbone')
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
    def __init__(self, clk: SimHandleBase, port: WishboneMasterPort, log: Logger|None):
        self._log = log or logger

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
                    self._log.debug(f'Read monitor word: {hex(int(word))}')
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
                self._log.debug(f'Monitor datastream: {dat.hex()}')
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
    def __init__(self, dut, period=(10, 'ns'), log: Logger|None=None):
        self._log = log or logger

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

        # This is currently unconnected in hardware (looped back to EB master)
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

        self.tx = WishboneMasterMonitor(dut.clk, self.tx_port, self._log)
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


PACKETS = OrderedDict({
    'probe':          ('4e6f11ff00000086', '4e6f164400000086'),

    'read_write_read_scratch0': {
        'read_scratch00': ('4e6f1044a00f00010000800000000000e80f00010000800100000004',
                        '4e6f1444060f010000008000000000000e0f01000000800100000000'),
        'write_scratch0': ('4e6f1044e80f010100000000001122330000800100000004',
                        '4e6f144400000000000000000e0f01000000800100000000'),
        'read_scratch01': ('4e6f1044a00f00010000800000000000e80f00010000800100000004',
                        '4e6f1444060f010000008000001122330e0f01000000800100000000'),
    },

    'write_read_scratch1': {
        'write_scratch1': ('4e6f1044e80f010100000004aabbccdd0000800100000004',
                        '4e6f144400000000000000000e0f01000000800100000000'),
        'read_scratch1':  ('4e6f1044a00f00010000800000000004e80f00010000800100000004',
                        '4e6f1444060f010000008000aabbccdd0e0f01000000800100000000'),
    },

    # Only valid if Scratchpad vendor ID ends with x"DEADBEEF"
    'read_sdb': ('4e6f1044a00f0001000080000000805ce80f00010000800100000004',
                 '4e6f1444060f010000008000deadbeef0e0f01000000800100000000'),

    # Only valid if SDB address = 0x00008000
    'read_cfg_sdb_addr': ('4e6f1044e80f0001000080000000000c',
                          '4e6f14440e0f01000000800000008000'),

    # Write IP to config space through WB master -> cfg. Then read from config
    # regs.
    'write_read_ip': {
        'write_ip': ('4e6f1044e80f0101000000387f0000010000800100000004',
                    '4e6f144400000000000000000e0f01000000800100000000'),
        'read_ip':  ('4e6f1044e80f00010000800000000018',
                    '4e6f14440e0f0100000080007f000001')
    },
})


@cocotb.coroutine
async def run_packet_test(dut, test_name, test):
    log = logger.getChild('PACKETS')
    log.setLevel('INFO')
    eb_dut = EtherboneDUT(dut, log=log)
    await eb_dut.reset()
    
    async def run(sub_name, pair):
        packet, expect = pair
        await eb_dut.send(bytes.fromhex(packet))
        resp = await eb_dut.recv()
        assert(resp.hex() == expect)
        log.info(f'Packet {test_name}.{sub_name} passed')

    if isinstance(test, dict):
        for k,v in test.items():
            await run(k, v)
    else:
        await run(test_name, test)

def generate_tests():
    factory = TestFactory(run_packet_test)
    factory.add_option(['test_name', 'test'], list(PACKETS.items()))
    factory.generate_tests()
# generate_tests()

@cocotb.test()
async def test_tmp(dut):
    eb_dut = EtherboneDUT(dut)
    await eb_dut.reset()

    inp = '4e6f1044a00f01000000800000000004'
    cocotb.start_soon(eb_dut.send(bytes.fromhex(inp)))

    await Timer(400, 'ns')
    
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    # assert(dut.rx_ack_o.value == 1)


@cocotb.test()
async def test_ack_on_rx(dut):
    eb_dut = EtherboneDUT(dut)
    await eb_dut.reset()

    probe = '4e6f11ff00000086'
    cocotb.start_soon(eb_dut.send(bytes.fromhex(probe)))

    await Timer(300, 'ns')
    
    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)
    assert(dut.rx_ack_o.value == 1)


@cocotb.test(skip=True)
# @cocotb.test(skip=False)
async def test_socket(dut):
    log = logger.getChild('Socket')
    eb_dut = EtherboneDUT(dut, log=log)
    await eb_dut.reset()

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('0.0.0.0', 0x1111))

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
