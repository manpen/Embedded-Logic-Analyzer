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
--   TODO: WRITE DESC FOR RS232.COMMON
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
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    
library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;

library RS232_CONTROLLER;

package RS232_CONTROLLER_PKG is
    type UART_WB_MOSI_T is record
        address : STD_LOGIC_VECTOR(1 downto 0);
        data    : STD_LOGIC_VECTOR(7 downto 0);
        stb     : STD_LOGIC;
        we      : STD_LOGIC;
    end record;
  
    type UART_WB_MISO_T is record
        data    : STD_LOGIC_VECTOR(7 downto 0);
        ack     : STD_LOGIC;
    end record;   

    component RS232_CONTROLLER_CORE is
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
            wb_i : in WB_MISO_T;
            wb_o : out WB_MOSI_T
        );
    end component;
    
    component RS232_FSM is
    port (
      -- uart interface
        uart_rx_int_i : in STD_LOGIC;
        uart_tx_int_i : in STD_LOGIC;
        uart_wb_i : in UART_WB_MISO_T;
        uart_wb_o : out UART_WB_MOSI_T;

      -- wishbone master interface
        wb_clk_i : in  STD_LOGIC;
        wb_rst_i : in  STD_LOGIC;
        wb_i : in  WB_MISO_T;
        wb_o : out WB_MOSI_T
    );
    end component;
    
    type SR_OP_T is (IDLE, RESET, LOAD, SHIFT, LOADSHIFT);
    component shift_register is
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
    end component shift_register;
    
    component hex_to_char is
        generic (
            uppercase_g : boolean := TRUE
        );
        port (
            hex_i : in  STD_LOGIC_VECTOR (3 downto 0);
            char_o : out  STD_LOGIC_VECTOR (7 downto 0)
        );
    end component hex_to_char;
    
    component char_to_hex is
        port (
            char_i : in  STD_LOGIC_VECTOR (7 downto 0);
            
            hex_o   : out STD_LOGIC_VECTOR (3 downto 0);
            valid_o : out STD_LOGIC
        );
    end component char_to_hex;
    
    component UART is
        generic(BRDIVISOR: INTEGER range 0 to 65535 := 130); -- Baud rate divisor
        port (
        -- Wishbone signals
            WB_CLK_I : in  std_logic;  -- clock
            WB_RST_I : in  std_logic;  -- Reset input
            WB_ADR_I : in  std_logic_vector(1 downto 0); -- Adress bus          
            WB_DAT_I : in  std_logic_vector(7 downto 0); -- DataIn Bus
            WB_DAT_O : out std_logic_vector(7 downto 0); -- DataOut Bus
            WB_WE_I  : in  std_logic;  -- Write Enable
            WB_STB_I : in  std_logic;  -- Strobe
            WB_ACK_O : out std_logic;	-- Acknowledge
        -- process signals     
            IntTx_O  : out std_logic;  -- Transmit interrupt: indicate waiting for Byte
            IntRx_O  : out std_logic;  -- Receive interrupt: indicate Byte received
            BR_Clk_I : in  std_logic;  -- Clock used for Transmit/Receive
            TxD_PAD_O: out std_logic;  -- Tx RS232 Line
            RxD_PAD_I: in  std_logic
        );    
    end component UART;

-- utils
    function MAX(a: integer; b: integer) RETURN integer;
    function is_eq_char(signal data : STD_LOGIC_VECTOR(7 downto 0); constant char : CHARACTER) return BOOLEAN;
    function TO_STD_LOGIC_VECTOR(constant char : CHARACTER) return STD_LOGIC_VECTOR;
end RS232_CONTROLLER_PKG;

package body RS232_CONTROLLER_PKG is
    function MAX(a: integer; b: integer) RETURN integer is
    begin
        if a > b then
            return a;
        else
            return b;
        end if;
    end MAX;
    
    function TO_STD_LOGIC_VECTOR(constant char : CHARACTER) return STD_LOGIC_VECTOR
    is begin
        return STD_LOGIC_VECTOR(TO_UNSIGNED(CHARACTER'pos(char), 8));
    end function;
    
    function is_eq_char (signal data : STD_LOGIC_VECTOR(7 downto 0); constant char : CHARACTER) return BOOLEAN
    is
        constant lc_a : STD_LOGIC_VECTOR(7 downto 0) := X"61";
        constant lc_z : STD_LOGIC_VECTOR(7 downto 0) := X"7A";
        variable uc_data, uc_char : STD_LOGIC_VECTOR(7 downto 0);
    begin
        if data = TO_STD_LOGIC_VECTOR(char) then
            return true;
        else
            uc_char := TO_STD_LOGIC_VECTOR(char);
            if uc_char >= lc_a and uc_char <= lc_z then
                uc_char := uc_char - X"20";
            end if;
            
            uc_data := data;
            if uc_data >= lc_a and uc_data <= lc_z then
                uc_data := uc_data - X"20";
            end if;
        
            return (uc_data = uc_char);
        end if;
    end function;
end RS232_CONTROLLER_PKG;
