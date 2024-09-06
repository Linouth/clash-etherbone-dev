-- Glue etherbone to the peripherals entity
-- This top entity should not make use of the t_wishbone types, as cocotb cannot
-- use those.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;
use work.etherbone_pkg.all;
use work.eb_internals_pkg.all;

entity playground_top is
  port(
    clk   : in std_logic;
    rst_n : in std_logic;

    -- RX Wishbone bus (slave)
    rx_adr_i : in std_logic_vector(c_wishbone_address_width-1 downto 0);
    rx_dat_i : in std_logic_vector(c_wishbone_data_width-1 downto 0);
    rx_we_i  : in std_logic;
    rx_sel_i : in std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    rx_stb_i : in std_logic;
    rx_cyc_i : in std_logic;

    rx_dat_o    : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    rx_ack_o    : out std_logic;
    rx_stall_o  : out std_logic;
    rx_rty_o    : out std_logic;
    rx_err_o    : out std_logic;

    -- TX Wishbone bus (master)
    tx_adr_o : out std_logic_vector(c_wishbone_address_width-1 downto 0);
    tx_dat_o : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    tx_we_o  : out std_logic;
    tx_sel_o : out std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    tx_stb_o : out std_logic;
    tx_cyc_o : out std_logic;

    tx_dat_i    : in std_logic_vector(c_wishbone_data_width-1 downto 0);
    tx_ack_i    : in std_logic;
    tx_stall_i  : in std_logic;
    tx_rty_i    : in std_logic;
    tx_err_i    : in std_logic;

    -- Configuration Wishbone bus (slave)
    cfg_adr_i : in std_logic_vector(c_wishbone_address_width-1 downto 0);
    cfg_dat_i : in std_logic_vector(c_wishbone_data_width-1 downto 0);
    cfg_we_i  : in std_logic;
    cfg_sel_i : in std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    cfg_stb_i : in std_logic;
    cfg_cyc_i : in std_logic;

    cfg_dat_o    : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    cfg_ack_o    : out std_logic;
    cfg_stall_o  : out std_logic;
    cfg_rty_o    : out std_logic;
    cfg_err_o    : out std_logic
  );
end playground_top;

architecture rtl of playground_top is
  signal eb_rx_i : t_wishbone_slave_in;
  signal eb_rx_o : t_wishbone_slave_out;
  signal eb_tx_i : t_wishbone_master_in;
  signal eb_tx_o : t_wishbone_master_out;

  signal eb_cfg_o : t_wishbone_slave_out;
  signal eb_cfg_i : t_wishbone_slave_in;

  signal wbm_o     : t_wishbone_master_out;
  signal wbm_i     : t_wishbone_master_in;

  signal s_my_mac: std_logic_vector(47 downto 0);
  signal s_my_ip: std_logic_vector(31 downto 0);
  signal s_my_port: std_logic_vector(15 downto 0);

  signal s_skip_stall : std_logic;
  signal s_skip_stb   : std_logic;

  constant c_sdb_address : t_wishbone_address := x"00008000";
begin
  -- Pretty sure these are just given so that you can look it up in the config
  -- registers. The Ethernet hardware driving the RX and TX lines are
  -- responsible to handle this... Right?
  -- Well no, the RX module does handle this info some how?
  -- s_my_mac <= (others => '0');
  -- s_my_ip <= x"7f000001";
  -- s_my_port <= x"1111";

  eb_rx_i.adr <= rx_adr_i;
  eb_rx_i.dat <= rx_dat_i;
  eb_rx_i.cyc <= rx_cyc_i;
  eb_rx_i.stb <= rx_stb_i;
  eb_rx_i.we  <= rx_we_i;
  eb_rx_i.sel <= rx_sel_i;
  rx_ack_o <= eb_rx_o.ack;
  rx_ack_o <= eb_rx_o.ack;
  rx_stall_o <= eb_rx_o.stall;
  rx_err_o <= eb_rx_o.err;
  rx_rty_o <= eb_rx_o.rty;

  tx_adr_o <= eb_tx_o.adr;
  tx_dat_o <= eb_tx_o.dat;
  tx_cyc_o <= eb_tx_o.cyc;
  tx_stb_o <= eb_tx_o.stb;
  tx_we_o <= eb_tx_o.we;
  tx_sel_o <= eb_tx_o.sel;
  eb_tx_i.ack <= tx_ack_i;
  eb_tx_i.stall <= tx_stall_i;
  eb_tx_i.err <= tx_err_i;
  eb_tx_i.rty <= tx_rty_i;

  eb_cfg_i.adr <= cfg_adr_i;
  eb_cfg_i.dat <= cfg_dat_i;
  eb_cfg_i.cyc <= cfg_cyc_i;
  eb_cfg_i.stb <= cfg_stb_i;
  eb_cfg_i.we  <= cfg_we_i;
  eb_cfg_i.sel <= cfg_sel_i;
  cfg_ack_o <= eb_cfg_o.ack;
  cfg_stall_o <= eb_cfg_o.stall;
  cfg_err_o <= eb_cfg_o.err;
  cfg_rty_o <= eb_cfg_o.rty;


  eb : eb_slave_top
  generic map(
    g_sdb_address    => c_sdb_address,
    g_timeout_cycles => 100)
  port map(
    clk_i        => clk,
    nRst_i       => rst_n,
    EB_RX_i      => eb_rx_i,
    EB_RX_o      => eb_rx_o,
    EB_TX_i      => eb_tx_i,
    EB_TX_o      => eb_tx_o,
    -- msi_slave_o  => open,
    -- msi_slave_i  => cc_dummy_slave_in,
    skip_stb_o   => open,
    skip_stall_i => s_skip_stall,

    WB_config_i  => eb_cfg_i,
    WB_config_o  => eb_cfg_o,

    WB_master_i  => wbm_i,
    WB_master_o  => wbm_o,

    my_mac_o     => s_my_mac,
    my_ip_o      => s_my_ip,
    my_port_o    => s_my_port);


  U_Peripherals : entity work.peripherals
  generic map(
    g_sdb_address    => c_sdb_address)
  port map(
    rst_n      => rst_n,
    clk        => clk,
    wb_i       => wbm_o,
    wb_o       => wbm_i
  );

end rtl;
