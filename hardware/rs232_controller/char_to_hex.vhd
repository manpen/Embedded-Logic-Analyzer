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
--   Simple combinatorial entity to decode char codes defined in
--   RS232_CONTROLLER.COMMON to hex.
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

entity char_to_hex is
    port (
        char_i : in  STD_LOGIC_VECTOR (7 downto 0);
        
        hex_o   : out STD_LOGIC_VECTOR (3 downto 0);
        valid_o : out STD_LOGIC
    );
end char_to_hex;

architecture Behavioral of char_to_hex is
begin
    process(char_i) is
    begin
           if is_eq_char(char_i, '0') then hex_o <= X"0"; valid_o <= '1';
        elsif is_eq_char(char_i, '1') then hex_o <= X"1"; valid_o <= '1';
        elsif is_eq_char(char_i, '2') then hex_o <= X"2"; valid_o <= '1';
        elsif is_eq_char(char_i, '3') then hex_o <= X"3"; valid_o <= '1';
        elsif is_eq_char(char_i, '4') then hex_o <= X"4"; valid_o <= '1';
        elsif is_eq_char(char_i, '5') then hex_o <= X"5"; valid_o <= '1';
        elsif is_eq_char(char_i, '6') then hex_o <= X"6"; valid_o <= '1';
        elsif is_eq_char(char_i, '7') then hex_o <= X"7"; valid_o <= '1';
        elsif is_eq_char(char_i, '8') then hex_o <= X"8"; valid_o <= '1';
        elsif is_eq_char(char_i, '9') then hex_o <= X"9"; valid_o <= '1';
        elsif is_eq_char(char_i, 'A') then hex_o <= X"A"; valid_o <= '1';
        elsif is_eq_char(char_i, 'B') then hex_o <= X"B"; valid_o <= '1';
        elsif is_eq_char(char_i, 'C') then hex_o <= X"C"; valid_o <= '1';
        elsif is_eq_char(char_i, 'D') then hex_o <= X"D"; valid_o <= '1';
        elsif is_eq_char(char_i, 'E') then hex_o <= X"E"; valid_o <= '1';
        elsif is_eq_char(char_i, 'F') then hex_o <= X"F"; valid_o <= '1';
        else                               hex_o <= X"0"; valid_o <= '0'; end if;
    end process;
end Behavioral;