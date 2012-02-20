--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   24.08.2011
--
-- Description:
--   This entity can be embedded into the logic analyzer core and is able
--   to overwrite the [sa_data_i]-input with a number of test pattern. Those can
--   be chose during runtime.
-- 
--   It is designed to be used while testing the logic anaylzer or its interfaces.
--
--   This module is inserted before the first stage of the sampling pipeline.
--   As it is fully synthesizable the pattern can be produced by the hardware
--   and then read and checked by the PC. It therefore gives the means to automatically
--   check the whole system including the connection to the computer and its drivers.
--
-- Pattern (available only if [bypass_g] = FALSE):
--   [pattern_selection_l] = X"00"
--    BYPASS. The input is passed through. However, as the output is registered, a
--    delay of 1 sa_clk-cycle is introduced.
--
--   [pattern_selection_l] = X"01"
--    This pattern assings each bank two values. The more significant slice of a bank is
--    addr_width_f(LA_CFG_BANK_COUNT_C) wide and holds the number of the bank. The less sig. slice
--    stores a counter. The counter is incremented each [sa_clk_i]-cycle. All banks have
--    the same counter value.
--
--      Sample: [LA_CFG_BANK_WIDTH_C]=6, [LA_CFG_BANK_COUNT_C]=3:   a 4 bit wide counter is used
--      sa_data_o = "10" & counter(3 downto 0) & "01" counter(3 downto 0) & "00" & counter(3 downto 0);
--                   ^^              ^^           ^^             ^^          ^^              ^^  
--               BANK IDENT        COUNTER    BANK IDENT       COUNTER   BANK IDENT        COUNTER
--
--    As this signal changes every sampling cycle, it should make live very hard for a
--    RLE and can be used as a "worst case check".
--
--   [pattern_selection_l] = X"02"
--    This test pattern is very similiar to the pattern described directly above. The only
--    difference is, that this pattern uses a wider counter, that is distributed over all banks.
--    
--      Sample [LA_CFG_BANK_WIDTH_C]=6, [LA_CFG_BANK_COUNT_C]=3:  a 12 bit wide counter is used
--      sa_data_o = "10" & counter(11 downto 8) & "01" & counter(7 downto 5) & "00" & counter(3 downto 0);
--                   ^^              ^^            ^^             ^^            ^^              ^^  
--               BANK IDENT        COUNTER      BANK IDENT       COUNTER     BANK IDENT        COUNTER
--      
--    It is obvious that the higher bank change less frequently than the lower. A
--    RLE compression is likely to exploid that property.
--
--  [pattern_selection_l] above X"02"
--    same as pattern 0: BYPASS
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
    use IEEE.MATH_REAL.ALL;
    
library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    use LOGIC_ANALYZER.TXT_UTIL.ALL;

entity TEST_PATTERN_GEN is
    port (
        sa_clk_i : in  STD_LOGIC;
          
        sa_data_i : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        sa_data_o : out STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        trg_data_o : out STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
        pattern_select_i : in STD_LOGIC_VECTOR(1 downto 0)
    );
end TEST_PATTERN_GEN;

architecture rtl of TEST_PATTERN_GEN is
    constant bank_addr_width_c : NATURAL := addr_width_f(LA_CFG_BANK_COUNT_C-1);
    constant counter_width_c : NATURAL := LA_CFG_BANK_WIDTH_C - bank_addr_width_c;
    
    signal counter_l : STD_LOGIC_VECTOR(counter_width_c * LA_CFG_BANK_COUNT_C + 3 downto 0) := (others => '0');
begin
    assert LA_CFG_BANK_WIDTH_C > bank_addr_width_c
    report "Test-Pattern Generator needs LA_CFG_BANK_WIDTH_C > log2(LA_CFG_BANK_COUNT_C)"
    severity failure;


    assert false
    report "Test-Pattern: Bank[" & str(LA_CFG_BANK_WIDTH_C-1) & " downto " & str(LA_CFG_BANK_WIDTH_C-bank_addr_width_c) & "] "
                    & "Counter[" & str(counter_width_c - 1) & " downto 0]" severity note;
                    
                  

-- PATTERN GENERATION
    process(sa_clk_i) is
        variable tmp : STD_LOGIC_VECTOR(sa_data_o'RANGE);
    begin
        if rising_edge(sa_clk_i) then
            tmp := (others => '-');
            case(pattern_select_i) is
                when "01" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'RANGE);
                    else
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l(counter_width_c - 1 downto 0);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                
                when "10" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'RANGE);
                    else                
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l( (i+1) * counter_width_c - 1 downto i * counter_width_c);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                    
                when "11" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'HIGH + 2 downto 2);
                    else
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l(counter_width_c + 1 downto 2);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                    
                when others =>
                    sa_data_o <= sa_data_i;
                    trg_data_o <= trg_data_i;
            end case;

            counter_l <= STD_LOGIC_VECTOR(UNSIGNED(counter_l) + TO_UNSIGNED(1, counter_l'LENGTH));
        end if;
    end process;
end rtl;