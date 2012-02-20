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
--   Simple combinatorial entity to decode hex to char codes defined
--   RS232_CONTROLLER.COMMON.
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

entity hex_to_char is
    generic (
        uppercase_g : boolean := TRUE
    );
    port (
        hex_i : in  STD_LOGIC_VECTOR (3 downto 0);
        char_o : out  STD_LOGIC_VECTOR (7 downto 0)
    );
end hex_to_char;

architecture Behavioral of hex_to_char is
begin
    char_o <=
        TO_STD_LOGIC_VECTOR('0') when hex_i=X"0" else
        TO_STD_LOGIC_VECTOR('1') when hex_i=X"1" else
        TO_STD_LOGIC_VECTOR('2')  when hex_i=X"2" else
        TO_STD_LOGIC_VECTOR('3')  when hex_i=X"3" else
        TO_STD_LOGIC_VECTOR('4')  when hex_i=X"4" else
        TO_STD_LOGIC_VECTOR('5')  when hex_i=X"5" else
        TO_STD_LOGIC_VECTOR('6')  when hex_i=X"6" else
        TO_STD_LOGIC_VECTOR('7')  when hex_i=X"7" else
        TO_STD_LOGIC_VECTOR('8')  when hex_i=X"8" else
        TO_STD_LOGIC_VECTOR('9')  when hex_i=X"9" else
        TO_STD_LOGIC_VECTOR('A')  when hex_i=X"A" and uppercase_g else
        TO_STD_LOGIC_VECTOR('B')  when hex_i=X"B" and uppercase_g else
        TO_STD_LOGIC_VECTOR('C')  when hex_i=X"C" and uppercase_g else
        TO_STD_LOGIC_VECTOR('D')  when hex_i=X"D" and uppercase_g else
        TO_STD_LOGIC_VECTOR('E')  when hex_i=X"E" and uppercase_g else
        TO_STD_LOGIC_VECTOR('F')  when hex_i=X"F" and uppercase_g else
        TO_STD_LOGIC_VECTOR('a')  when hex_i=X"A" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('b')  when hex_i=X"B" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('c')  when hex_i=X"C" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('d')  when hex_i=X"D" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('e')  when hex_i=X"E" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('f') ;
end Behavioral;

