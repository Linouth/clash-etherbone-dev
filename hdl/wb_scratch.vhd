library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.wishbone_pkg.all;


entity wb_scratch is

  generic (
    g_interface_mode      : t_wishbone_interface_mode      := CLASSIC;
    g_address_granularity : t_wishbone_address_granularity := WORD;
    g_num_regs : integer);
  port(
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
end wb_scratch;

architecture behaviour of wb_scratch is
  type regmap_t is array (0 to g_num_regs-1) of std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal regs : regmap_t;

  signal s_ack : std_logic;
begin

  process(clk, rst_n)
    variable v_index : integer;
  begin
    v_index := to_integer(unsigned(wb_adr_i));

    if rst_n = '0' then
      regs <= (others => (others => '0'));
    elsif rising_edge(clk) then
      if wb_stb_i = '1' and wb_cyc_i = '1' then
        -- Latches

        if wb_we_i = '0' then
          -- Read request 
          wb_dat_o <= regs(v_index);
        else
          -- Write request
          regs(v_index) <= wb_dat_i;
        end if;

        s_ack <= '1';
      else
        wb_dat_o <= (others => 'Z');
        s_ack  <= '0';
      end if;
    end if;
  end process;

  --Wishbone interface
  wb_stall_o <= '0';
  wb_ack_o <= s_ack;
end behaviour;
