library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity playground_top is
  port(
    clk   : in std_logic;
    rst_n : in std_logic;

    -- RX Wishbone bus (slave)
    rx_adr_i : in std_logic_vector(31 downto 0);
    rx_dat_i : in std_logic_vector(31 downto 0);
    rx_we_i  : in std_logic;
    rx_sel_i : in std_logic_vector(3 downto 0);
    rx_stb_i : in std_logic;
    rx_cyc_i : in std_logic;

    rx_dat_o    : out std_logic_vector(31 downto 0);
    rx_ack_o    : out std_logic;
    rx_stall_o  : out std_logic;
    rx_rty_o    : out std_logic;
    rx_err_o    : out std_logic;

    -- TX Wishbone bus (master)
    tx_adr_o : out std_logic_vector(31 downto 0);
    tx_dat_o : out std_logic_vector(31 downto 0);
    tx_we_o  : out std_logic;
    tx_sel_o : out std_logic_vector(3 downto 0);
    tx_stb_o : out std_logic;
    tx_cyc_o : out std_logic;

    tx_dat_i    : in std_logic_vector(31 downto 0);
    tx_ack_i    : in std_logic;
    tx_stall_i  : in std_logic;
    tx_rty_i    : in std_logic;
    tx_err_i    : in std_logic;

    -- Configuration Wishbone bus (slave) (Obsolete?)
    cfg_adr_i : in std_logic_vector(31 downto 0);
    cfg_dat_i : in std_logic_vector(31 downto 0);
    cfg_we_i  : in std_logic;
    cfg_sel_i : in std_logic_vector(3 downto 0);
    cfg_stb_i : in std_logic;
    cfg_cyc_i : in std_logic;

    cfg_dat_o    : out std_logic_vector(31 downto 0);
    cfg_ack_o    : out std_logic;
    cfg_stall_o  : out std_logic;
    cfg_rty_o    : out std_logic;
    cfg_err_o    : out std_logic
  );
end playground_top;


architecture rtl of playground_top is
  signal s_rx_o_ACK,
         s_rx_o_ERR,
         s_rx_o_STALL,
         s_rx_o_RTY,
         s_tx_o_CYC,
         s_tx_o_STB,
         s_tx_o_WE : boolean;
begin

  U_Clash : entity work.topEntity
  port map(
    clk => clk,
    rst => not rst_n,
    en => true,

    rx_i_ADR      => rx_adr_i,
    rx_i_DAT_MOSI => rx_dat_i,
    rx_i_SEL      => rx_sel_i,
    rx_i_LOCK     => false,
    rx_i_CYC      => rx_cyc_i = '1',
    rx_i_STB      => rx_stb_i = '1',
    rx_i_WE       => rx_we_i = '1',
    rx_i_CTI      => (others => '0'),
    rx_i_BTE      => (others => '0'),

    tx_i_DAT_MISO => tx_dat_i,
    tx_i_ACK      => tx_ack_i = '1',
    tx_i_ERR      => tx_err_i = '1',
    tx_i_STALL    => tx_stall_i = '1',
    tx_i_RTY      => tx_rty_i = '1',

    rx_o_DAT_MISO => rx_dat_o,
    rx_o_ACK      => s_rx_o_ACK,
    rx_o_ERR      => s_rx_o_ERR,
    rx_o_STALL    => s_rx_o_STALL,
    rx_o_RTY      => s_rx_o_RTY,

    tx_o_ADR      => tx_adr_o,
    tx_o_DAT_MOSI => tx_dat_o,
    tx_o_SEL      => tx_sel_o,
    tx_o_LOCK     => open,
    tx_o_CYC      => s_tx_o_CYC,
    tx_o_STB      => s_tx_o_STB,
    tx_o_WE       => s_tx_o_WE,
    tx_o_CTI      => open,
    tx_o_BTE      => open
  );

  rx_ack_o <= '1' when s_rx_o_ACK else '0';
  rx_err_o <= '1' when s_rx_o_ERR else '0';
  rx_stall_o <= '1' when s_rx_o_STALL else '0';
  rx_rty_o <= '1' when s_rx_o_RTY else '0';

  tx_cyc_o <= '1' when s_tx_o_CYC else '0';
  tx_stb_o <= '1' when s_tx_o_STB else '0';
  tx_we_o <= '1' when s_tx_o_WE else '0';
end rtl;
