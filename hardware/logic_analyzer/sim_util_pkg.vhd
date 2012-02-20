--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   23.08.2011
--
-- Description:
--   Simulation helper
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
    use IEEE.STD_LOGIC_1164.all;

package SIM_UTIL_PKG is
    -- controls wishbone master's STB_O, WE_O and waits
    -- for ACK_I on the rising edge of WB_CLK_I
    procedure sim_wb_master_handshake(constant wev : STD_LOGIC;
                                      signal clk : in STD_LOGIC;
                                      signal stb, we : out STD_LOGIC;
                                      signal ack : in STD_LOGIC);
                               
    -- generates one clock cycle of <period> length and with an
    -- duty cycle of 50%
    procedure sim_clk_cycle(constant period : time;
                            signal   clk    : out STD_LOGIC);
                            
    constant  SIM_DEF_CLK_PERIOD : time := 10.0 ns; -- 100 MHz
    procedure sim_clk_cycle(signal   clk    : out STD_LOGIC);
    
    -- returns real value of time in ns
    function to_real(value : time) return real;
    
    procedure sim_test_skipped(text: string);
    procedure sim_test_okay(text: string);
    procedure sim_test_failed(text: string);
    procedure sim_tb_skipped(text: string);
    procedure sim_tb_finished(text: string);
end SIM_UTIL_PKG;

package body SIM_UTIL_PKG is
    procedure sim_clk_cycle(signal clk : out STD_LOGIC)
    is begin
        sim_clk_cycle(SIM_DEF_CLK_PERIOD, clk);
    end procedure;

    procedure sim_clk_cycle(constant period : time;
                        signal   clk    : out STD_LOGIC
    ) is
    begin
        clk <= '0';
        wait for period/2;
        clk <= '1';
        wait for period/2;
    end procedure;

    procedure sim_wb_master_handshake(
        constant wev : STD_LOGIC;
        signal clk : in STD_LOGIC;
        signal stb, we : out STD_LOGIC;
        signal ack : in STD_LOGIC
    ) is
    begin
        wait for 1 ps;
        stb <= '1';
        we  <= wev;

        if clk='1' then
            wait until clk = '0';
        end if;
        wait until clk = '1';
        
        while(ack /= '1') loop
            if clk='1' then
                wait until clk = '0';
            end if;
            wait until clk = '1';
        end loop;
        
        stb <= '0';
        we  <= '0';
    end procedure;
    
    function to_real(value : time) return real
    is begin
        return real(value / 1 ns);
    end function;
    
    procedure sim_test_skipped(text: string) is begin report ">TEST_SKIPPED<: " & text severity warning; end procedure;
    procedure sim_test_okay(text: string)    is begin report ">TEST_OKAY<: "    & text severity note;    end procedure;
    procedure sim_test_failed(text: string)  is begin report ">TEST_FAILED<: "  & text severity error;   end procedure;
    procedure sim_tb_skipped(text: string)   is begin report ">TB_SKIPPED<: "   & text severity warning; end procedure;
    procedure sim_tb_finished(text: string)  is begin report ">TB_FINISHED<: "  & text severity note;    end procedure;
end SIM_UTIL_PKG;
