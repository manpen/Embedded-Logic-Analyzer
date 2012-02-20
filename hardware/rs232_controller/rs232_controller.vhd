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

entity RS232_CONTROLLER_CORE is
    generic (
        uart_br_divisor : integer := 1302
    );
    port (
      -- rs232 
        txd_pad_o : out STD_LOGIC;
        rxd_pad_i : in  STD_LOGIC;
        
      -- wishbone master interface
        wb_clk_i : in  STD_LOGIC;
        wb_rst_i : in  STD_LOGIC;
        wb_i : in  WB_MISO_T;
        wb_o : out WB_MOSI_T
    );
end RS232_CONTROLLER_CORE;

architecture Behavioral of RS232_CONTROLLER_CORE is
    signal uart_rx_int_l, uart_tx_int_l : STD_LOGIC;
    signal uart_wb_mosi_l : UART_WB_MOSI_T;
    signal uart_wb_miso_l : UART_WB_MISO_T;
begin
    inst_fsm: RS232_FSM 
    port map (
        uart_wb_i => uart_wb_miso_l,
        uart_wb_o => uart_wb_mosi_l,
        uart_rx_int_i => uart_rx_int_l,
        uart_tx_int_i => uart_tx_int_l,
    
        wb_clk_i => wb_clk_i,
        wb_rst_i => wb_rst_i,
        wb_i => wb_i,
        wb_o => wb_o
    );

    inst_uart : UART
    generic map (uart_br_divisor)
    port map (
        BR_Clk_I => wb_clk_i,
        TxD_PAD_O => txd_pad_o,
        RxD_PAD_I => rxd_pad_i,
        
        WB_CLK_I => wb_clk_i,
        WB_RST_I => wb_rst_i,
        WB_ADR_I => uart_wb_mosi_l.address,
        WB_DAT_I => uart_wb_mosi_l.data,
        WB_WE_I  => uart_wb_mosi_l.we,
        WB_STB_I => uart_wb_mosi_l.stb,
        WB_ACK_O => uart_wb_miso_l.ack,
        WB_DAT_O => uart_wb_miso_l.data,
    
        IntRx_O => uart_rx_int_l,
        IntTx_O => uart_tx_int_l
    );
end Behavioral;