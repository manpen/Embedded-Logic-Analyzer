--------------------------------------------------------------------------------
-- Company: 
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
-- Engineer: 
--   Manuel Penschuck - manuel at penschuck dot eu
-- Create Date:
--   05.08.2011
--
-- Description:
--   Driver for the 4-digit 7-segment display on the Digilent Sparta 3
--   board.
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
    use IEEE.STD_LOGIC_UNSIGNED.ALL;    
    
entity SEGMENT7 is
    generic (
        clk_divisor_g : POSITIVE := 1
    );
    port (
        clk_i  : in  STD_LOGIC;
        data_i : in  STD_LOGIC_VECTOR (15 downto 0);
        
        segment_o : out STD_LOGIC_VECTOR (6 downto 0);
        digit_o   : out STD_LOGIC_VECTOR (3 downto 0)
    );
end SEGMENT7;

architecture RTL of SEGMENT7 is
    signal shift_reg_l    : STD_LOGIC_VECTOR(data_i'RANGE);
    signal digit_select_l : STD_LOGIC_VECTOR(3 downto 0) := "1000";
    signal wait_reg_l     : STD_LOGIC_VECTOR(31 downto 0) := (others => '0');
    
    signal clk_div_l      : STD_LOGIC := '0';
    signal clk_div_counter_l : INTEGER RANGE 0 to clk_divisor_g + 1 := 0;
begin
    clk_div: process(clk_i) is
    begin
        if rising_edge(clk_i) then
            clk_div_counter_l <= clk_div_counter_l + 1;
            if clk_div_counter_l = clk_divisor_g then
                clk_div_counter_l <= 0;
                clk_div_l <= not clk_div_l;
            end if;
        end if;
    end process;

    output: process(clk_div_l) is
    begin
        if rising_edge(clk_div_l) then
            if digit_select_l = "1000" and wait_reg_l = 0 then
                shift_reg_l <= data_i;
                digit_select_l <= "0001";
                wait_reg_l <= (0 => '1', others => '0');
            else
                if (wait_reg_l = 0) then
                    shift_reg_l <= "0000" & shift_reg_l(15 downto 4);
                    digit_select_l <= digit_select_l(2 downto 0) & "0";
                    wait_reg_l <= (0 => '1', others => '0');
                else
                    wait_reg_l <= wait_reg_l(wait_reg_l'high-1 downto 0) & "0";
                end if;
            end if;
        end if;
    end process;
    
 -- outputs
    digit_o <= "1111" when wait_reg_l = 1 else not digit_select_l ;
    segment_o <=
        "1111111" when wait_reg_l = 0 else
        "1000000" when shift_reg_l(3 downto 0) = X"0" else
        "1111001" when shift_reg_l(3 downto 0) = X"1" else
        "0100100" when shift_reg_l(3 downto 0) = X"2" else
        "0110000" when shift_reg_l(3 downto 0) = X"3" else
        "0011001" when shift_reg_l(3 downto 0) = X"4" else
        "0010010" when shift_reg_l(3 downto 0) = X"5" else
        "0000011" when shift_reg_l(3 downto 0) = X"6" else
        "1111000" when shift_reg_l(3 downto 0) = X"7" else
        "0000000" when shift_reg_l(3 downto 0) = X"8" else
        "0011000" when shift_reg_l(3 downto 0) = X"9" else
        "0001000" when shift_reg_l(3 downto 0) = X"A" else
        "0000000" when shift_reg_l(3 downto 0) = X"B" else
        "1000110" when shift_reg_l(3 downto 0) = X"C" else
        "1000100" when shift_reg_l(3 downto 0) = X"D" else
        "0000110" when shift_reg_l(3 downto 0) = X"E" else
        "0001110";
end RTL;