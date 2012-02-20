---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   14.09.2011
--
-- Description:
--   The input multiplexer allows to select one input bank that is injected into the
--   sampling pipeline. Illegal [bank_select_i] result in unknown values on the output.
--   The same behaivour can be emulated by the input shift register, however the isr
--   need significant more hardware resources. Therefore this module can be used if the
--   additional features of the isr are not needed.
--
--   The change indicator is asserted directly after a reset or if the bank or its value change.
--   It can be used by the encoding units to compress the data. If its not used it should be
--   optimized away by the synthezisier.
-- 
--   Once a trigger event is received it is propagated with 1 cycle delay (do keep correlated with
--   data) and hold until reset.
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
---------------------------------------------------------------------------------------------------
library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;

entity INPUT_MULTIPLEXER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_IN_T;
        stage_o     : out MUX_STAGE_OUT_T;
        
    -- Config
        bank_select_i  : in STD_LOGIC_VECTOR(addr_width_f(LA_CFG_BANK_COUNT_C) - 1 downto 0)
    );
end INPUT_MULTIPLEXER;

architecture RTL of INPUT_MULTIPLEXER is
begin
    process (sa_clk_i) is
        variable post_reset_v : STD_LOGIC := '1';
        variable trigger_v : STD_LOGIC := '0';
        variable old_bank_v : STD_LOGIC_VECTOR(bank_select_i'RANGE);
        variable selected_v, old_v : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C - 1 downto 0);
    begin
        stage_o.new_cycle <= '1';
    
        if rising_edge(sa_clk_i) then
        -- delay values by one clock cycle to maintain correlation
            stage_o.reset <= stage_i.reset;
			trigger_v := (not stage_i.reset) AND (trigger_v or stage_i.trigger);
            stage_o.trigger <= trigger_v;

        -- select active bank
            selected_v := (others => '-'); -- prevent latch
            for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                if STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_select_i'LENGTH)) = bank_select_i then
                    selected_v := stage_i.data((i+1)* selected_v'LENGTH - 1 downto i*selected_v'LENGTH);
                end if;
            end loop;
            
            -- synopsys translate_off
            assert to_integer(unsigned(bank_select_i)) < LA_CFG_BANK_COUNT_C
                report "MUX: invalid bank selected" severity error;
            -- synopsys translate_on

            stage_o.data <= selected_v;

        -- detect changes
            stage_o.changed <= active_high_c(post_reset_v = '1' or old_v /= selected_v or old_bank_v /= bank_select_i);
            old_v := selected_v;
            old_bank_v := bank_select_i;
            post_reset_v := stage_i.reset;
        end if;
    end process;
end RTL;