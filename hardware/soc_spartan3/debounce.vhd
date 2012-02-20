---------------------------------------------------------------------------------------------------
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
--   Simple Button Debounce Algorithm, that can be applied to multiple
--   buttons [width_g]. It uses a shift register and changes the detected
--   state per button as soon as the last [deb_length_g] gathered values
--   are constant. Use a combination of [deb_length_g] and [clk_i] frequency
--   that takes at least 30ms into account.
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
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    
library SOC_SPARTAN3;

entity DEBOUNCE is
    generic (
        width_g : NATURAL;
        deb_length_g : POSITIVE := 8
    );
    port (
        clk_i : in  STD_LOGIC;
        data_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
        data_o : out  STD_LOGIC_VECTOR(width_g - 1 downto 0)
    );
end DEBOUNCE;

architecture Behavioral of DEBOUNCE is
    constant history_zero_c : STD_LOGIC_VECTOR(deb_length_g - 1 downto 0) := (others => '0');
    type   DEB_HISTORY_T is array(data_i'RANGE) of STD_LOGIC_VECTOR(deb_length_g - 1 downto 0);
begin
    process(clk_i) is
        variable data_history_v : DEB_HISTORY_T := (others => (others => '0'));
        variable counter_v : integer range 0 to (2**15) - 1 := 0;
    begin
        if rising_edge(clk_i) then
            if counter_v = 0 then
                for btn in data_i'RANGE loop
                    if data_history_v(btn) = history_zero_c then
                        data_o(btn) <= '0';
                    elsif (not data_history_v(btn)) = history_zero_c then
                        data_o(btn) <= '1';
                    end if;
                    data_history_v(btn) := data_history_v(btn)(data_history_v(btn)'HIGH - 1 downto 0) & data_i(btn);
                end loop;
            end if;
            counter_v := counter_v + 1;
        end if;
    end process;
end Behavioral;