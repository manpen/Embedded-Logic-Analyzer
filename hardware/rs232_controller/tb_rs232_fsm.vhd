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
--   TODO: WRITE DESC FOR RS232_CONTROLLER
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
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;

library RS232_CONTROLLER;
    use RS232_CONTROLLER.RS232_CONTROLLER_PKG.ALL;
    use RS232_CONTROLLER.ucrc_pkg.ALL;

entity TB_RS232_FSM is
end TB_RS232_FSM;
 
architecture behavior of TB_RS232_FSM is 
   --Inputs
   signal uart_rx_int_i : STD_LOGIC := '0';
   signal uart_tx_int_i : STD_LOGIC := '0';
   signal uart_wb_i : UART_WB_MISO_T := ((others => '0'), '0');
   signal wb_clk_i : STD_LOGIC := '0';
   signal wb_rst_i : STD_LOGIC := '0';
   signal wb_i : WB_MISO_T := ((others => '0'), '0');

 	--Outputs
   signal uart_wb_o : UART_WB_MOSI_T;
   signal wb_o : WB_MOSI_T;

   constant clk_period : time := 10 ns;
BEGIN
	-- Instantiate the Unit Under Test (UUT)
   uut: RS232_FSM PORT MAP (
      uart_rx_int_i => uart_rx_int_i,
      uart_tx_int_i => uart_tx_int_i,
      uart_wb_i => uart_wb_i,
      uart_wb_o => uart_wb_o,
      wb_clk_i => wb_clk_i,
      wb_rst_i => wb_rst_i,
      wb_i => wb_i,
      wb_o => wb_o
   );

   wb_clk_i_process :process
   begin
		wb_clk_i <= '0';
		wait for clk_period/2;
		wb_clk_i <= '1';
		wait for clk_period/2;
   end process;
 

   -- Stimulus process
   stim_proc: process
   begin		
      wb_rst_i <= '1';
      wait for clk_period*2;
      wb_rst_i <= '0';
      wait for clk_period;
   
   -- SET ADDRESS
      uart_wb_i.data <= TO_STD_LOGIC_VECTOR('X');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      wait for clk_period * 10;

      uart_wb_i.data <= TO_STD_LOGIC_VECTOR('1');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      wait for clk_period * 10;

      uart_wb_i.data <= TO_STD_LOGIC_VECTOR('2');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      wait for clk_period * 10;

      uart_wb_i.data <= TO_STD_LOGIC_VECTOR('1');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      wait for clk_period * 10;

      uart_wb_i.data <= TO_STD_LOGIC_VECTOR(';');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      wait for clk_period * 10;

      assert wb_o.address = X"12" report "address not properly set";
      
   -- READ
      uart_wb_i.data <= TO_STD_LOGIC_VECTOR('r');
      uart_rx_int_i <= '1';                       wait until uart_wb_o.stb='1'; wait for clk_period * 0.1;
      uart_rx_int_i <= '0'; uart_wb_i.ack <= '1'; wait until uart_wb_o.stb='0'; wait for clk_period * 0.1;
      uart_wb_i.ack <= '0';

      wb_i.data <= X"AB";
      wb_i.ack <= '0'; wait until wb_o.stb = '1'; wait for clk_period * 0.1;
      wb_i.ack <= '1'; wait until wb_o.stb = '0'; wait for clk_period * 0.1;
      wb_i.ack <= '0';
     
      while(uart_wb_o.data /= TO_STD_LOGIC_VECTOR('+')) loop
          uart_wb_i.ack <= '0'; if not uart_wb_o.stb = '1' then wait until uart_wb_o.stb = '1'; end if; wait for clk_period * 0.1;
          uart_wb_i.ack <= '1'; uart_tx_int_i <= '0'; if not uart_wb_o.stb = '0' then wait until uart_wb_o.stb = '0'; end if; wait for clk_period * 0.1;
          uart_wb_i.ack <= '0'; wait for clk_period * 10;
          uart_tx_int_i <= '1';
      end loop;
      
      
      



      wait;
   end process;
END;