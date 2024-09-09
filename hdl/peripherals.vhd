library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;

entity peripherals is
  generic(
    g_sdb_address  : t_wishbone_address := x"00008000");
  port(
    clk   : in std_logic;
    rst_n : in std_logic;

    wb_cfg_i   : out t_wishbone_slave_in;
    wb_cfg_o   : in t_wishbone_slave_out;

    wb_i    : in t_wishbone_slave_in;
    wb_o    : out t_wishbone_slave_out
  );
end peripherals;

architecture rtl of peripherals is
  constant c_scratch_sdb : t_sdb_device := (
    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000000",
      addr_last   => x"000000000000000f",
      product     => (
        vendor_id => x"00000000DEADBEEF",
        device_id => x"ff07fc47",
        version   => x"00000001",
        date      => x"20120305",
        name      => "Scratchpad         ")));

  constant c_ticks_sdb : t_sdb_device := (
    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000010",
      addr_last   => x"000000000000001f",
      product     => (
        vendor_id => x"0000000000000000",
        device_id => x"00000000",
        version   => x"00000001",
        date      => x"00000000",
        name      => "Ticks              ")));

  constant c_cfg_sdb : t_sdb_device := (
    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000020",
      addr_last   => x"000000000000003f",
      product     => (
        vendor_id => x"0000000000000000",
        device_id => x"00000000",
        version   => x"00000001",
        date      => x"00000000",
        name      => "EB configuration   ")));


  constant c_periph_count : natural := 3;


  constant c_layout : t_sdb_record_array(c_periph_count-1 downto 0) :=
    ( 0 => f_sdb_embed_device(c_scratch_sdb, x"00000000")
    , 1 => f_sdb_embed_device(c_ticks_sdb, x"00000010")
    , 2 => f_sdb_embed_device(c_cfg_sdb, x"00000020")
    );

  signal wb_scratch_i : t_wishbone_slave_in;
  signal wb_scratch_o : t_wishbone_slave_out;
  signal wb_ticks_i : t_wishbone_slave_in;
  signal wb_ticks_o : t_wishbone_slave_out;

  signal cross_slaves_i : t_wishbone_slave_in_array (c_periph_count-1 downto 0);
  signal cross_slaves_o : t_wishbone_slave_out_array(c_periph_count-1 downto 0);

begin

  U_Tics : entity work.wb_tics
  generic map (
    g_period              => 64)
  port map (
    rst_n_i    => rst_n,
    clk_sys_i  => clk,
    wb_adr_i   => wb_ticks_i.adr(7 downto 4),
    wb_dat_i   => wb_ticks_i.dat,
    wb_dat_o   => wb_ticks_o.dat,
    wb_cyc_i   => wb_ticks_i.cyc,
    wb_sel_i   => wb_ticks_i.sel,
    wb_stb_i   => wb_ticks_i.stb,
    wb_we_i    => wb_ticks_i.we,
    wb_ack_o   => wb_ticks_o.ack,
    wb_stall_o => wb_ticks_o.stall
  );

  U_Scratch : entity work.wb_scratch
  generic map (
    g_num_regs => 4) -- Should be 2**adr'length
  port map (
    rst_n      => rst_n,
    clk        => clk,

    wb_i      => wb_scratch_i,
    wb_o      => wb_scratch_o
  );

  U_crossbar : xwb_sdb_crossbar
    generic map(
      g_verbose     => true,
      g_num_masters => 1,
      g_num_slaves  => c_periph_count,
      g_registered  => true,
      g_wraparound  => true,
      g_layout      => c_layout,
      g_sdb_addr    => g_sdb_address
      )
    port map(
      clk_sys_i  => clk,
      rst_n_i    => rst_n,
      -- Master connections (INTERCON is a slave)
      slave_i(0) => wb_i,
      slave_o(0) => wb_o,
      -- Slave connections (INTERCON is a master)
      master_i => cross_slaves_o,
      master_o => cross_slaves_i
      );

    cross_slaves_o(0) <= wb_scratch_o;
    wb_scratch_i      <= cross_slaves_i(0);
    cross_slaves_o(1) <= wb_ticks_o;
    wb_ticks_i        <= cross_slaves_i(1);
    cross_slaves_o(2) <= wb_cfg_o;
    wb_cfg_i          <= cross_slaves_i(2);

end rtl;
