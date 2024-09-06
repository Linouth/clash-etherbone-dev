import logging
import itertools
from pprint import pprint
import asyncio
import socket

import cocotb
from cocotb.utils import get_sim_time
from cocotb.clock import Clock
from cocotb.triggers import Timer, RisingEdge

from cocotbext.wishbone.driver import WishboneMaster, WBOp
from cocotbext.wishbone.monitor import WishboneSlave

logger = logging.getLogger('Wishbone test')
logger.setLevel(logging.DEBUG)

class MyWishboneMaster(WishboneMaster):
    _signals = ["cyc_i", "stb_i", "we_i", "adr_i", "dat_i", "dat_o", "ack_o"]
    _optional_signals = ["sel_o", "err_o", "stall_o", "rty_o"]

async def init_test(dut):
    dut.rst_n.value = 1

    dut.rx_adr_i.value = 0
    dut.rx_dat_i.value = 0
    dut.rx_we_i.value = 0
    dut.rx_stb_i.value = 0
    dut.rx_cyc_i.value = 0
    dut.rx_sel_i.value = 0xF

    dut.cfg_adr_i.value = 0
    dut.cfg_dat_i.value = 0
    dut.cfg_we_i.value = 0
    dut.cfg_stb_i.value = 0
    dut.cfg_cyc_i.value = 0
    dut.cfg_sel_i.value = 0xF

    clock = Clock(dut.clk, 10, units='ns')
    cocotb.start_soon(clock.start())


    dut.rst_n.value = 0
    await Timer(205, units='ns')
    dut.rst_n.value = 1
    await Timer(205, units='ns')

@cocotb.test(skip=True)
async def first_test(dut):
    await init_test(dut)


def str_to_ints(s, n=2):
    pairs = [ ''.join(x) for x in itertools.batched(s, n) ]
    return [ int(x, base=16) for x in pairs ]


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

    dut.tx_dat_i.value = 0
    dut.tx_ack_i.value = 0
    dut.tx_stall_i.value = 0
    dut.tx_rty_i.value = 0
    dut.tx_err_i.value = 0

    await init_test(dut)

    await Timer(100, 'ns')

    '''
    For the time being, lets just check a raw 'probe' packet. Taken from eb-ls
    to a netcat socket.

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

def read_word(dut) -> int|str:
    if dut.tx_cyc_o.value and not dut.tx_stall_i.value:
        if dut.tx_stb_o.value:
            dut.tx_ack_i.value = 1
            out = int(dut.tx_dat_o)
            logger.debug(f'Read tx_dat_o: {hex(out)}')
        else:
            out = 'cyc'
    else:
        out = 'stb'
        dut.tx_ack_i.value = 0

    return out

async def read_data(dut):
    buf = []

    started = False
    while True:
        await RisingEdge(dut.clk)

        word = read_word(dut)

        if isinstance(word, str):
            if word == 'cyc':
                continue
            elif word == 'stb':
                if not started:
                    continue
                else:
                    break
            else:
                raise ValueError('Unknown return value??')
        started = True

        buf.append(word)

    out = b''.join([
        x.to_bytes(4) for x in buf
    ])
    logger.debug(f'TX datastream: {out.hex()}')
    return out


@cocotb.test(skip=False)
async def test_socket(dut):
    edge = RisingEdge(dut.clk)

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.bind(('0.0.0.0', 0x1111))

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

    wb_cfg = WishboneMaster(dut, "cfg", dut.clk,
                            width=32,   # size of data bus
                            timeout=10, # in clock cycle number
                            signals_dict={"cyc":  "cyc_i",
                                        "stb":  "stb_i",
                                        "we":   "we_i",
                                        "adr":  "adr_i",
                                        "datwr":"dat_i",
                                        "datrd":"dat_o",
                                        "ack":  "ack_o" })


    dut.tx_dat_i.value = 0
    dut.tx_ack_i.value = 0
    dut.tx_stall_i.value = 0
    dut.tx_rty_i.value = 0
    dut.tx_err_i.value = 0

    await init_test(dut)

    await Timer(100, 'ns')

    # for i in range(8):
    while True:
        data, addr = sock.recvfrom(1024)
        data = data.hex()
        print('udp got', data)

        cocotb.start_soon(wb_rx.send_cycle([
            WBOp(0, x) for x in str_to_ints(data, 8)
        ]))

        resp = await read_data(dut)

        sock.sendto(resp, addr)
        print('sent', resp)

        await Timer(100, 'ns')


    sock.close()
