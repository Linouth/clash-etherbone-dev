import logging
import itertools

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import Timer

from cocotbext.wishbone.driver import WishboneMaster, WBOp

logger = logging.getLogger('Wishbone test')
logger.setLevel(logging.DEBUG)

async def init_test(dut):
    dut.rst_n.value = 1

    # dut.wb_adr_i.value = 0
    # dut.wb_dat_i.value = 0
    # dut.wb_we_i.value = 0
    # dut.wb_stb_i.value = 0
    # dut.wb_cyc_i.value = 0
    # dut.wb_sel_i.value = 0xF

    clock = Clock(dut.clk, 7, units='ns')
    cocotb.start_soon(clock.start())

    dut.rst_n.value = 0
    await Timer(205, units='ns')
    dut.rst_n.value = 1
    await Timer(205, units='ns')

@cocotb.test(skip=True)
async def first_test(dut):
    await init_test(dut)


def bytestr_to_bytes(s, n=2):
    pairs = [ ''.join(x) for x in itertools.batched(s, n) ]
    return [ bytes.fromhex(x) for x in pairs ]


@cocotb.test()
async def test_etherbone(dut):
    wbs = WishboneMaster(dut, "rx", dut.clk,
                         width=32,   # size of data bus
                         timeout=10, # in clock cycle number
                         signals_dict={"cyc":  "cyc_i",
                                     "stb":  "stb_i",
                                     "we":   "we_i",
                                     "adr":  "adr_i",
                                     "datwr":"dat_i",
                                     "datrd":"dat_o",
                                     "ack":  "ack_o" })

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

    PROBE_STR = 'c0ed11110010fe234e6f11ff00000086'
    PROBE = bytestr_to_bytes('c0ed11110010fe234e6f11ff00000086')
    # await wbs.send_cycle([WBOp(0b10, bytes.fromhex(PROBE_STR))])

    await wbs.send_cycle([
        WBOp(0b10, int.from_bytes(x)) for x in bytestr_to_bytes(PROBE_STR, 2)
    ])

    # await Timer(100, 'ns')
