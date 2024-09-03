library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.wishbone_pkg.all;

entity peripherals is
  port(
    clk   : in std_logic;
    rst_n : in std_logic;

    -- wb_i    : in t_wishbone_slave_in;
    -- wb_o    : out t_wishbone_slave_out;

    wb_adr_i : in std_logic_vector(c_wishbone_address_width-1 downto 0);
    wb_dat_i : in std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_we_i  : in std_logic;
    wb_sel_i : in std_logic_vector(c_wishbone_data_width/8-1 downto 0);
    wb_stb_i : in std_logic;
    wb_cyc_i : in std_logic;

    wb_dat_o    : out std_logic_vector(c_wishbone_data_width-1 downto 0);
    wb_ack_o    : out std_logic;
    wb_stall_o  : out std_logic;
    wb_rty_o    : out std_logic;
    wb_err_o    : out std_logic
  );
end peripherals;

architecture rtl of peripherals is
  component wb_tics
    generic (
      g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD;
      g_period : integer);
    port (
      rst_n_i    : in  std_logic;
      clk_sys_i  : in  std_logic;
      wb_adr_i   : in  std_logic_vector(3 downto 0);
      wb_dat_i   : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o   : out std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_cyc_i   : in  std_logic;
      wb_sel_i   : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0);
      wb_stb_i   : in  std_logic;
      wb_we_i    : in  std_logic;
      wb_ack_o   : out std_logic;
      wb_stall_o : out std_logic);
  end component;

  component wb_scratch
    generic (
      g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
      g_address_granularity : t_wishbone_address_granularity := WORD;
      g_num_regs : integer);
    port (
      rst_n : in std_logic;
      clk : in std_logic;

      wb_adr_i : in  std_logic_vector(1 downto 0);
      wb_dat_i : in  std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_dat_o : out std_logic_vector(c_wishbone_data_width-1 downto 0);
      wb_cyc_i  : in  std_logic;
      wb_sel_i  : in  std_logic_vector(c_wishbone_data_width/8-1 downto 0);
      wb_stb_i  : in  std_logic;
      wb_we_i   : in  std_logic;
      wb_ack_o  : out std_logic;
      wb_stall_o: out std_logic
      );
  end component;
begin

  -- U_Tics : wb_tics
  -- generic map (
  --   g_period              => 64)
  -- port map (
  --   rst_n_i    => rst_n,
  --   clk_sys_i  => clk,
  --   wb_adr_i   => wb_adr_i(3 downto 0),
  --   wb_dat_i   => wb_dat_i,
  --   wb_dat_o   => wb_dat_o,
  --   wb_cyc_i   => wb_cyc_i,
  --   wb_sel_i   => wb_sel_i,
  --   wb_stb_i   => wb_stb_i,
  --   wb_we_i    => wb_we_i,
  --   wb_ack_o   => wb_ack_o,
  --   wb_stall_o => wb_stall_o);

  wb_err_o <= '0';
  wb_rty_o <= '0';

  U_Scratch : wb_scratch
  generic map (
    g_num_regs => 4) -- Should be 2**adr'length
  port map (
    rst_n      => rst_n,
    clk        => clk,
    wb_adr_i   => wb_adr_i(1 downto 0),
    wb_dat_i   => wb_dat_i,
    wb_dat_o   => wb_dat_o,
    wb_cyc_i   => wb_cyc_i,
    wb_sel_i   => wb_sel_i,
    wb_stb_i   => wb_stb_i,
    wb_we_i    => wb_we_i,
    wb_ack_o   => wb_ack_o,
    wb_stall_o => wb_stall_o);

end rtl;
