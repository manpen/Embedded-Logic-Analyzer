--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   18.08.2011
--
-- Description:
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

library RS232_CONTROLLER;
    use RS232_CONTROLLER.RS232_CONTROLLER_PKG.ALL;

entity shift_register is
    generic (
        nibbles_g : positive := 8
    );
    port ( 
        clk_i  : in STD_LOGIC;
        
        operation_i : in SR_OP_T;
        
        shift_data_i : in STD_LOGIC_VECTOR(3 downto 0);
        
        data_i : in  STD_LOGIC_VECTOR (4*nibbles_g-5 downto 0);
        data_o : out STD_LOGIC_VECTOR (4*nibbles_g-5 downto 0)
    );
end shift_register;

architecture Behavioral of shift_register is
    signal data_l : STD_LOGIC_VECTOR(4*nibbles_g-1 downto 0);
begin
    process(clk_i) is 
    begin
        if rising_edge(clk_i) then
            case(operation_i) is
                when RESET => data_l <= (others => '0');
                when LOAD  => data_l <= data_i & "0000";
                when SHIFT => data_l <= data_l(data_l'HIGH - 4 downto 0) & "0000";
                when LOADSHIFT => data_l <= data_l(data_l'HIGH - 4 downto 0) & shift_data_i;
                when others => NULL;
            end case;
        end if;
    end process;
    
    data_o <= data_l(data_l'HIGH downto 4);
end Behavioral;