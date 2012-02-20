---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   17.08.2011
--
-- Description:
--   Edge trigger. The trigger fires if the wire masked by [mask_i] undergo
--   the state change indicated by [edge_i] within one clock cycle.
--   
--   Each input bit has to be assigned to one of the following three groups:
--     - mask_i(i) = '0', edge_i(i) = dont care: Ignore this bit
--     - mask_i(i) = '1', edge_i(i) = '0': Trigger on falling edge
--     - mask_i(i) = '1', edge_i(i) = '1': Trigger on rising edge
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

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    
entity TRIGGER_EDGE is
    port ( 
        sa_clk_i  : in  STD_LOGIC;
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        edge_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        mask_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trigger_o : out STD_LOGIC
    );
end TRIGGER_EDGE;

architecture RTL of TRIGGER_EDGE is
    signal trg_data_old_l : STD_LOGIC_VECTOR(trg_data_i'RANGE);
begin
    process(sa_clk_i) is
        variable
            change_p,
            change_rising_p,
            change_falling_p : STD_LOGIC_VECTOR(trg_data_i'RANGE);
            
        variable result_p : STD_LOGIC;
    begin
        if rising_edge(sa_clk_i) then
            -- store current data layout 
            trg_data_old_l <= trg_data_i;
            
            -- calculate changes
            change_p := trg_data_i xor trg_data_old_l;
            change_rising_p := change_p and trg_data_i;
            change_falling_p := change_p and (not trg_data_i);
            
            result_p := '1';
            for i in change_p'RANGE loop
                result_p := result_p and 
                    ( (change_rising_p(i) and edge_i(i))
                      or (change_falling_p(i) or not edge_i(i))
                      or not mask_i(i) );
            end loop;
            
            trigger_o <= result_p;
        end if;
    end process;
end RTL;