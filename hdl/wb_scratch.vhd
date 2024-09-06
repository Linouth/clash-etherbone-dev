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

    wb_i    : in t_wishbone_slave_in;
    wb_o    : out t_wishbone_slave_out
    );
end wb_scratch;

architecture behaviour of wb_scratch is
  type regmap_t is array (0 to g_num_regs-1) of std_logic_vector(c_wishbone_data_width-1 downto 0);
  signal regs : regmap_t;

  signal s_ack : std_logic;

  signal r_stb : std_logic;
begin

  process(clk, rst_n)
    variable v_index : integer;
  begin
    if rst_n = '0' then
      regs <= (others => (others => '0'));
    elsif rising_edge(clk) then
      r_stb <= wb_i.stb;

      if wb_i.stb = '1' and wb_i.cyc = '1' then
        v_index := to_integer(unsigned(wb_i.adr(31 downto 2)));

        -- Latches
        if v_index < g_num_regs then
          if wb_i.we = '0' then
            -- Read request 
            wb_o.dat <= regs(v_index);
          else
            -- Write request
            regs(v_index) <= wb_i.dat;
          end if;
        else
          wb_o.dat <= x"AAAAAAAA";
        end if;
      else
        wb_o.dat <= (others => '0');
      end if;
    end if;
  end process;

  --Wishbone interface
  wb_o.stall <= '0';
  wb_o.ack <= r_stb and wb_i.cyc;
end behaviour;
