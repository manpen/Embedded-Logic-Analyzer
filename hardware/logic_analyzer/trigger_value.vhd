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
--   Parametrisierbarer Trigger fuer Werte. Mittels [data_width_g] kann die
--   Anzahl der Eingaenge zur Synthese bestimmt werden. Waehrend der Laufzeit
--   koennen mittels [value_i] und [mask_i] (high aktiv)  
--   die Trigger-Bedingungen bestimmt werden.
--   
--   Um ein Trigger-Ereigniss auszuloesen, muessen alle Bedingungen erfuellt
--   sein. Die folgenden Betrachtungen gelten jeweils fuer ein Signal j:
--
--   mask_i(j) = '0': Das Signal wird ignoriert
--   mask_i(j) = '1': Wenn data_i(j) /= value_i(j) wird ein Trigger verhindert
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
    
entity TRIGGER_VALUE is
    port ( 
        sa_clk_i  : in STD_LOGIC;
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        value_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        mask_i  : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trigger_o : out STD_LOGIC
    );
end TRIGGER_VALUE;

architecture RTL of TRIGGER_VALUE is
begin
    process(sa_clk_i) is
        variable result_p : STD_LOGIC;
        variable equal_p : STD_LOGIC_VECTOR(trg_data_i'RANGE);
    begin
        if rising_edge(sa_clk_i) then
            result_p := '1';
            
            -- bitwise equal
            equal_p := (trg_data_i and value_i) or not (trg_data_i or value_i);
            
            for i in trg_data_i'RANGE loop
                result_p := result_p and (not mask_i(i) or equal_p(i));
            end loop;
            
            trigger_o <= result_p;
        end if;
    end process;
end RTL;