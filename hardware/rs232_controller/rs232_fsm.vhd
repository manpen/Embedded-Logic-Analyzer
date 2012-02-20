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

entity RS232_FSM is
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
end RS232_FSM;

architecture Behavioral of RS232_FSM is
    constant nibbles_c : positive := integer(ceil(real(MAX(wb_o.data'LENGTH, wb_o.address'LENGTH)) / 4.0));
    constant data_nibbles_c : positive := integer(ceil(real(wb_o.data'LENGTH) / 4.0));
    constant sr_zero_c : STD_LOGIC_VECTOR(4*nibbles_c - 1 downto 0) := (others => '0');

    type BOOL_TO_LOGIC_T is array(BOOLEAN) of STD_LOGIC;
    constant active_high_c : BOOL_TO_LOGIC_T := (false => '0', true => '1');

    type FSM_RS_MODE_T is (rs_data, rs_addr, rs_length);
    type FSM_STATE_T is (fsm_idle, fsm_idle_wait_rx, fsm_idle_wait_tx,
                         fsm_success, fsm_fail,

                         fsm_rs_set_mode_addr, fsm_rs_set_mode_data, fsm_rs_set_mode_length,
                         fsm_rs_wait_rx, fsm_rs_wait_wb, fsm_rs_shift,
                         fsm_rs_latch_addr, fsm_rs_latch_length, fsm_rs_latch_data, 
                         fsm_wb_write_wait, fsm_wb_write_next_reg,
                         
                         fsm_read_set_single, fsm_read_set_block,
                         fsm_read_wb_wait, fsm_read_latch, fsm_read_crc, 
                         fsm_read_uart_wb_wait, fsm_read_uart_tx_wait, fsm_read_shift, fsm_read_next_reg,
                         fsm_read_wait_wb_crc, fsm_read_wait_tx_crc
                         );
    type CRC_MUX_T is (CRC_MUX_CMD_ADDR, CRC_MUX_CMD_DATA, CRC_MUX_CMD_LEN, CRC_MUX_SR, CRC_MUX_C2H);
    type H2C_MUX_T is (H2C_MUX_SR, H2C_MUX_CRC);
    type UART_MUX_T is (UART_MUX_PLUS, UART_MUX_MINUS, UART_MUX_H2C);
    
    type WB_FSM_T is record
        state : FSM_STATE_T;
        rs_mode : FSM_RS_MODE_T;
        nibbles : integer range 0 to data_nibbles_c-1;
        bytes   : integer range 0 to (2**wb_o.address'LENGTH) - 1;
    
        block_length : STD_LOGIC_VECTOR(wb_o.address'RANGE);
    
        sr_op : SR_OP_T;

        crc_reset : STD_LOGIC;
        crc_ena   : STD_LOGIC;
        crc_mux   : CRC_MUX_T;
        
        h2c_mux   : H2C_MUX_T;
        
        uart_stb, uart_we : STD_LOGIC;
        uart_mux  : UART_MUX_T;
        
        wb_stb, wb_we : STD_LOGIC;
        wb_address : STD_LOGIC_VECTOR(wb_o.address'RANGE);
    end record;

    signal fsm_l, fsm_next_l : WB_FSM_T;

  -- SHIFT REGISTER
    signal sr_data_in_l, sr_data_out_l : STD_LOGIC_VECTOR(4*nibbles_c - 1 downto 0);
    
  -- CRC
    signal crc_data_in_l, crc_data_in_rev_l, crc_crc_l : STD_LOGIC_VECTOR(3 downto 0);
    signal crc_match_l : STD_LOGIC;
    
  -- CHAR <-> HEX
    signal c2h_valid_l : STD_LOGIC;
    signal c2h_value_l, h2c_in_l : STD_LOGIC_VECTOR(3 downto 0);
    signal h2c_out_l : STD_LOGIC_VECTOR(7 downto 0);
begin
    fsm_next_state: process(fsm_l, crc_match_l, uart_wb_i, uart_rx_int_i, uart_tx_int_i, c2h_valid_l, wb_i) is
    begin
        fsm_next_l.state <= fsm_l.state;
    
        case(fsm_l.state) is
            when fsm_idle =>
                if uart_rx_int_i = '1' then
                    fsm_next_l.state <= fsm_idle_wait_rx;
                end if;
            
            when fsm_idle_wait_rx =>
                if uart_wb_i.ack = '1' then
                       if is_eq_char(uart_wb_i.data, 'x') then fsm_next_l.state <= fsm_rs_set_mode_addr;
                    elsif is_eq_char(uart_wb_i.data, 'y') then fsm_next_l.state <= fsm_rs_set_mode_data;
                    elsif is_eq_char(uart_wb_i.data, 'z') then fsm_next_l.state <= fsm_rs_set_mode_length;
                    elsif is_eq_char(uart_wb_i.data, 'r') then fsm_next_l.state <= fsm_read_set_single;
                    elsif is_eq_char(uart_wb_i.data, 'm') then fsm_next_l.state <= fsm_read_set_block;
                    else fsm_next_l.state <= fsm_fail; end if;
                end if;
            
            -- used when transmitting status info before returning to wait
            when fsm_idle_wait_tx =>
                if uart_rx_int_i = '1' then
                    fsm_next_l.state <= fsm_idle;
                end if;
            
            when fsm_success =>
                if uart_wb_i.ack = '1' and uart_tx_int_i = '0' then
                    fsm_next_l.state <= fsm_idle_wait_tx;
                end if;
            
            when fsm_fail =>
                if uart_wb_i.ack = '1' and uart_tx_int_i = '0' then
                    fsm_next_l.state <= fsm_idle_wait_tx;
                end if;
    
    -- REGISTER WRITE ACCESS (wb address, block length, wb single write)
            when fsm_rs_set_mode_addr | fsm_rs_set_mode_data | fsm_rs_set_mode_length => 
                fsm_next_l.state <= fsm_rs_wait_rx;
            
            when fsm_rs_wait_rx => 
                if uart_rx_int_i = '1' then
                    fsm_next_l.state <= fsm_rs_wait_wb;
                end if;
            
            when fsm_rs_wait_wb =>
                if uart_wb_i.ack = '1' then
                    if c2h_valid_l = '1' then 
                        fsm_next_l.state <= fsm_rs_shift;
                    elsif is_eq_char(uart_wb_i.data, ';') and crc_match_l = '1' then
                        case(fsm_l.rs_mode) is
                            when rs_addr  => fsm_next_l.state <= fsm_rs_latch_addr;
                            when rs_data  => fsm_next_l.state <= fsm_rs_latch_data;
                            when rs_length => fsm_next_l.state <= fsm_rs_latch_length;
                        end case;
                    else
                        fsm_next_l.state <= fsm_fail;
                    end if;
                end if;
            
            when fsm_rs_shift => 
                fsm_next_l.state <= fsm_rs_wait_rx;
                
            when fsm_rs_latch_addr =>
                fsm_next_l.state <= fsm_success;
                
            when fsm_rs_latch_length =>
                fsm_next_l.state <= fsm_success;

            when fsm_rs_latch_data =>
                fsm_next_l.state <= fsm_wb_write_wait;
            
            when fsm_wb_write_wait =>
                if wb_i.ack='1' then
                    fsm_next_l.state <= fsm_wb_write_next_reg;
                end if;
                
            when fsm_wb_write_next_reg =>
                fsm_next_l.state <= fsm_success;


        -- READ FROM WISHBONE BUS
            when fsm_read_set_single | fsm_read_set_block =>
                fsm_next_l.state <= fsm_read_wb_wait;
                
            when fsm_read_wb_wait =>
                if wb_i.ack='1' then
                    fsm_next_l.state <= fsm_read_latch;
                end if;
                
            when fsm_read_latch =>
                fsm_next_l.state <= fsm_read_crc;
                
            when fsm_read_crc =>
                if uart_wb_i.ack='1' and uart_tx_int_i = '0' then
                    fsm_next_l.state <= fsm_read_uart_tx_wait;
                else
                    fsm_next_l.state <= fsm_read_uart_wb_wait;
                end if;

            when fsm_read_uart_wb_wait =>
                if uart_wb_i.ack='1' and uart_tx_int_i = '0' then
                    fsm_next_l.state <= fsm_read_uart_tx_wait;
                end if;            
            
            when fsm_read_uart_tx_wait =>
                if uart_tx_int_i = '1' then
                    if fsm_l.nibbles /= 0 then
                        fsm_next_l.state <= fsm_read_shift;
                    else
                        fsm_next_l.state <= fsm_read_next_reg;
                    end if;
                end if;
                
            when fsm_read_shift =>
                fsm_next_l.state <= fsm_read_crc;
            
            when fsm_read_next_reg =>
                if fsm_l.bytes = 0 then
                    fsm_next_l.state <= fsm_read_wait_wb_crc;
                else
                    fsm_next_l.state <= fsm_read_wb_wait;
                end if;
            
            when fsm_read_wait_wb_crc =>
                if uart_wb_i.ack='1' and uart_tx_int_i = '0' then
                    fsm_next_l.state <= fsm_read_wait_tx_crc;
                end if;            

            when fsm_read_wait_tx_crc =>
                if uart_tx_int_i = '1' then
                    fsm_next_l.state <= fsm_success;
                end if;
        end case;
    end process;
  
  -- FSM INTERNAL
    fsm_next_l.rs_mode <=
        rs_addr   when fsm_next_l.state = fsm_rs_set_mode_addr   else
        rs_data   when fsm_next_l.state = fsm_rs_set_mode_data   else
        rs_length when fsm_next_l.state = fsm_rs_set_mode_length else
        fsm_l.rs_mode;
        
    fsm_next_l.nibbles <=
        data_nibbles_c - 1 when fsm_next_l.state = fsm_read_wb_wait else
        fsm_l.nibbles - 1  when fsm_next_l.state = fsm_read_shift else
        fsm_l.nibbles;
        
    fsm_next_l.bytes <=
        TO_INTEGER(UNSIGNED(fsm_l.block_length)) when fsm_next_l.state = fsm_read_set_block else
        1                              when fsm_next_l.state = fsm_read_set_single else
        fsm_l.bytes - 1                when fsm_next_l.state = fsm_read_next_reg else
        fsm_l.bytes;
        
     fsm_next_l.block_length <=
        sr_data_out_l(fsm_next_l.block_length'RANGE) when fsm_next_l.state = fsm_rs_latch_length else
        fsm_l.block_length;
  
  -- SHIFT REGISTER  
    fsm_next_l.sr_op <=
        reset     when fsm_next_l.state = fsm_idle else
        loadshift when fsm_next_l.state = fsm_rs_shift else
        load      when fsm_next_l.state = fsm_read_latch else
        shift     when fsm_next_l.state = fsm_read_shift else
        idle;

  -- CRC
    fsm_next_l.crc_reset <= active_high_c(fsm_next_l.state = fsm_idle);
    fsm_next_l.crc_ena <= 
        '1' when fsm_next_l.state = fsm_rs_set_mode_addr else 
        '1' when fsm_next_l.state = fsm_rs_set_mode_data else 
        '1' when fsm_next_l.state = fsm_rs_set_mode_length else 
        '1' when fsm_next_l.state = fsm_rs_shift else 
        '1' when fsm_next_l.state = fsm_read_crc else
        '0';
        
    fsm_next_l.crc_mux <=
        CRC_MUX_CMD_ADDR when fsm_next_l.state = fsm_rs_set_mode_addr else
        CRC_MUX_CMD_DATA when fsm_next_l.state = fsm_rs_set_mode_data else
        CRC_MUX_CMD_LEN when fsm_next_l.state = fsm_rs_set_mode_length else
        CRC_MUX_SR when fsm_next_l.state = fsm_read_crc else
        CRC_MUX_C2H;

    fsm_next_l.h2c_mux <=
        H2C_MUX_CRC when fsm_next_l.state = fsm_read_wait_wb_crc else
        H2C_MUX_SR;

  -- UART WB
    fsm_next_l.uart_mux <=
        UART_MUX_PLUS  when fsm_next_l.state = fsm_success else
        UART_MUX_MINUS when fsm_next_l.state = fsm_fail else
        UART_MUX_H2C;
        
    fsm_next_l.uart_we <=
        '1' when fsm_next_l.state = fsm_success else
        '1' when fsm_next_l.state = fsm_fail else
        '1' when fsm_next_l.state = fsm_read_uart_wb_wait else
        '1' when fsm_next_l.state = fsm_read_wait_wb_crc else
        '0';
        
    fsm_next_l.uart_stb <=
        -- reads
        '1' when fsm_next_l.state = fsm_idle_wait_rx else
        '1' when fsm_next_l.state = fsm_rs_wait_wb else
        -- writes
        '1' when fsm_next_l.state = fsm_success else
        '1' when fsm_next_l.state = fsm_fail else
        '1' when fsm_next_l.state = fsm_read_uart_wb_wait else
        '1' when fsm_next_l.state = fsm_read_wait_wb_crc else
        -- idle
        '0';
    
  -- EXTERNAL WISHBONE 
    fsm_next_l.wb_address <=
        sr_data_out_l(fsm_next_l.wb_address'RANGE) when fsm_next_l.state = fsm_rs_latch_addr else
        STD_LOGIC_VECTOR(UNSIGNED(fsm_l.wb_address) + TO_UNSIGNED(1, 1)) when fsm_next_l.state = fsm_wb_write_next_reg else
        STD_LOGIC_VECTOR(UNSIGNED(fsm_l.wb_address) + TO_UNSIGNED(1, 1)) when fsm_next_l.state = fsm_read_next_reg else
        fsm_l.wb_address;
        
    fsm_next_l.wb_stb <=
        '1' when fsm_next_l.state = fsm_wb_write_wait else
        '1' when fsm_next_l.state = fsm_read_wb_wait else
        '0';
        
    fsm_next_l.wb_we <= active_high_c(fsm_next_l.state=fsm_wb_write_wait);

-- synchronisation
    fsm_sync : process(wb_clk_i)
    is begin
        if rising_edge(wb_clk_i) then
            fsm_l <= fsm_next_l;
            if wb_rst_i = '1' then
                fsm_l <= (
                    state => fsm_idle,
                    rs_mode => rs_addr, 
                    nibbles => 0,
                    bytes   => 0,
                    block_length => (0 => '1', others => '0'),
                    sr_op => RESET,
                    crc_reset => '1',
                    crc_ena   => '0',
                    crc_mux   => CRC_MUX_C2H,
                    h2c_mux    => H2C_MUX_SR,
                    uart_stb => '0',
                    uart_we => '0',
                    uart_mux  => UART_MUX_MINUS,
                    wb_stb => '0',
                    wb_we => '0',
                    wb_address => (others => '0')
                );
            end if;
        end if;
    end process;
   -- wishbone bus
    wb_o.stb <= fsm_l.wb_stb;
    wb_o.we  <= fsm_l.wb_we;
    wb_o.address <= fsm_l.wb_address;
    wb_o.data <= sr_data_out_l(wb_o.data'RANGE);
    
   -- uart wishbone bus    
    uart_wb_o.stb <= fsm_l.uart_stb;
    uart_wb_o.we <= fsm_l.uart_we;
    uart_wb_o.address <= "00"; -- only access data register
    uart_wb_o.data <=
        TO_STD_LOGIC_VECTOR('+') when fsm_l.uart_mux = UART_MUX_PLUS else
        TO_STD_LOGIC_VECTOR('-') when fsm_l.uart_mux = UART_MUX_MINUS else
        h2c_out_l;    
    

-- COMPONENTS
    
    inst_sr: SHIFT_REGISTER
    generic map (nibbles_c + 1)
    port map (
        clk_i => wb_clk_i,
        operation_i => fsm_l.sr_op,
        shift_data_i => c2h_value_l,
        data_i => sr_data_in_l,
        data_o => sr_data_out_l
    );
    sr_data_in_l(wb_i.data'RANGE) <= wb_i.data;
    sr_data_in_l(sr_data_in_l'HIGH downto wb_i.data'HIGH + 1) <= (others => '0');
    
    inst_crc : UCRC_PAR
    generic map (
        POLYNOMIAL => "1011",
        INIT_VALUE => "0000",
        DATA_WIDTH => 4,
        SYNC_RESET => 1
    )
    port map (
        clk_i => wb_clk_i,
        rst_i => fsm_l.crc_reset,
        clken_i => fsm_l.crc_ena,
        data_i => crc_data_in_rev_l,
        match_o => crc_match_l,
        crc_o => crc_crc_l
    );
    crc_data_in_rev_l <= crc_data_in_l(0) & crc_data_in_l(1) & crc_data_in_l(2) & crc_data_in_l(3);
    crc_data_in_l <= 
        "0001"                                                          when fsm_l.crc_mux = CRC_MUX_CMD_ADDR else
        "1101"                                                          when fsm_l.crc_mux = CRC_MUX_CMD_DATA else
        "0110"                                                          when fsm_l.crc_mux = CRC_MUX_CMD_LEN else
        sr_data_out_l(data_nibbles_c*4-1 downto data_nibbles_c*4-4)     when fsm_l.crc_mux = CRC_MUX_SR else
        c2h_value_l;
    
    hex_decode: char_to_hex
    port map (
        char_i  => uart_wb_i.data,
        hex_o   => c2h_value_l,
        valid_o => c2h_valid_l
    );
    
    char_encode: hex_to_char
    port map (
        hex_i =>  h2c_in_l,
        char_o => h2c_out_l
    );    
    h2c_in_l <= sr_data_out_l(data_nibbles_c*4-1 downto data_nibbles_c*4-4) when fsm_l.h2c_mux = H2C_MUX_SR else
                crc_crc_l;
end Behavioral;
