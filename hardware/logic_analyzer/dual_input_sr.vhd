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
--   Simple dual input shift register that allows to load upto two values parrallely.
--   Loaded values are clocked out with [clk_i], whereas valid_o is asserted until 
--   the shift register runs empty. No data once in the sr will be overwritten. So
--   if there's not enough space in the sr, only ena1 will be loaded, if both inputs
--   are provided.
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

entity dual_input_sr is
    generic (
        width_g : NATURAL
    );

    port (
        clk_i : in  STD_LOGIC;
        reset_i : in STD_LOGIC;
        
        input1_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
        ena1_i : STD_LOGIC;
        
        input2_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
        ena2_i : in STD_LOGIC;
        
        output_o : out STD_LOGIC_VECTOR(width_g - 1 downto 0);
        valid_o : out STD_LOGIC
    );
end dual_input_sr;

architecture Behavioral of dual_input_sr is
    type SR_T is array(1 downto 0) of STD_LOGIC_VECTOR(width_g - 1 downto 0);
    signal sr_l : SR_T;
    signal sr_level_l : integer range 0 to 2 := 0; -- stores how many valid values there are in the sr
begin
    process(clk_i) is
    begin
        if rising_edge(clk_i) then
            if reset_i = '1' then
                sr_level_l <= 0;
                sr_l <= (others => (others => '-'));
                valid_o <= '0';
            else
                valid_o <= '1';
                
                if sr_level_l = 1 or sr_level_l = 0 then
                    -- can load input1 and input2
                    if ena1_i = '1' and ena2_i = '1' then
                        sr_l <= (0 => input1_i, 1 => input2_i);
                        sr_level_l <= 2;
                    elsif ena1_i = '1' then
                        sr_l <= (0 => input1_i, 1 => (others => '-'));
                        sr_level_l <= 1;
                    elsif ena2_i = '1' then
                        sr_l <= (0 => input2_i, 1 => (others => '-'));
                        sr_level_l <= 1;
                    else
                        sr_l <= (others => (others => '-'));
                        sr_level_l <= 0;
                        valid_o <= '0';
                    end if;
                elsif sr_level_l = 2 then
                    if ena1_i = '1' then
                        sr_l <= (0 => sr_l(1), 1 => input1_i);
                        sr_level_l <= 2;
                    elsif ena2_i = '1' then
                        sr_l <= (0 => sr_l(1), 1 => input2_i);
                        sr_level_l <= 2;
                    else
                        sr_l <= (0 => sr_l(1), 1 => (others => '-'));
                        sr_level_l <= 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    output_o <= sr_l(0);
end Behavioral;