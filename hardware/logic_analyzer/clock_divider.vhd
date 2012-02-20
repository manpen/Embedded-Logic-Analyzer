--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   11.09.2011
--
-- Description:
--   This module can be used to divide the sampling clock frequency by an
--   dynamical fraction. A counter is incremented on each rising edge of
--   [sa_clk_i]. If it becomes bigger than the threshold value [threshold_i]
--   it is reset. The output is asserted while the counter is zero. Therefore
--   an very assymmetrical signal is generated for big threshold values.
--
-- Additional Comments:
--   This program is free software: you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation, either version 3 of the License, or
--   (at your option) any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------
library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;

entity CLOCK_DIVIDER is
    port (
        sa_clk_i : in  STD_LOGIC;
        sa_clk_o : out  STD_LOGIC;
        
        threshold_i  : in STD_LOGIC_VECTOR(max_f(0, LA_CFG_CLKDIV_WIDTH_C-1) downto 0)
    );
end CLOCK_DIVIDER;

architecture Behavioral of clock_divider is
    signal counter_l, threshold_l : STD_LOGIC_VECTOR(LA_CFG_CLKDIV_WIDTH_C-1 downto 0) := (others => '0');
    signal sa_clk_l : STD_LOGIC;
begin
    sa_clk_o <= sa_clk_i when threshold_i = STD_LOGIC_VECTOR(TO_UNSIGNED(0, counter_l'LENGTH)) else sa_clk_l;

    process(sa_clk_i) is
    begin
        if rising_edge(sa_clk_i) then
            threshold_l <= threshold_i;
            if UNSIGNED(counter_l) = UNSIGNED(threshold_l) then
                counter_l <= (others => '0');
                sa_clk_l <= '1';
            else
                counter_l <= STD_LOGIC_VECTOR(UNSIGNED(counter_l) + TO_UNSIGNED(1, counter_l'LENGTH));
                sa_clk_l <= '0';
            end if;
        end if;
    end process;
end Behavioral;