library IEEE;
    use IEEE.STD_LOGIC_1164.all;
    use IEEE.NUMERIC_STD.ALL;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    
library JTAG_CONTROLLER;
    use JTAG_CONTROLLER.JTAG_CONTROLLER_PKG.all;
    
entity JTAG_CONTROLLER_CORE is
    port (
      -- jtag
        jtag_state_i : in JTAG_STATE_T;
        jtag_op_reg_i : in JTAG_REGISTER_MOSI_T;
        jtag_op_reg_tdo_o : out STD_LOGIC;
        jtag_data_reg_i : in JTAG_REGISTER_MOSI_T;
        jtag_data_reg_tdo_o : out STD_LOGIC;
    
    
      -- wishbone master interface
        wb_clk_i : in  STD_LOGIC;
        wb_rst_i : in  STD_LOGIC;
        wb_i : in WB_MISO_T;
        wb_o : out WB_MOSI_T
    );
end JTAG_CONTROLLER_CORE;

architecture Behav of JTAG_CONTROLLER_CORE is
    constant DEFAULT_WIDTH_C : POSITIVE := max_f(LA_CFG_WB_DATA_WIDTH_C, LA_CFG_WB_ADDR_WIDTH_C)+1;
    constant DEFAULT_WIDTH_REP_C : STD_LOGIC_VECTOR(7 downto 0) := STD_LOGIC_VECTOR(TO_UNSIGNED(DEFAULT_WIDTH_C, 8));
    
    constant LENGTH_REP_C : STD_LOGIC_VECTOR(5 downto 0) := STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_JTAG_MAX_BLOCK_C, 6));
  
-- OPERATION REGISTER
    type OPERATION_T is (READ_WIDTH1, READ_WIDTH2,
                         SET_ADDR, READ_BLOCK_LEN, 
                         REG_READ, REG_WRITE,
                         BLOCK_READ);
    type OPERATION_BIN_T is array(OPERATION_T) of STD_LOGIC_VECTOR(3 downto 0);
    
    -- lower 3 bits encode command, upper bit is ODD parity
    constant OP_READ_WIDTH1 : STD_LOGIC_VECTOR(3 downto 0) := "1000";
    constant OP_READ_WIDTH2 : STD_LOGIC_VECTOR(3 downto 0) := "0001";
    constant OP_SET_ADDR : STD_LOGIC_VECTOR(3 downto 0) := "0010";
    constant OP_READ_BLOCK_LEN : STD_LOGIC_VECTOR(3 downto 0) := "1011";
    constant OP_REG_READ : STD_LOGIC_VECTOR(3 downto 0) := "0100";
    constant OP_REG_WRITE : STD_LOGIC_VECTOR(3 downto 0) := "1101";
    constant OP_BLOCK_READ : STD_LOGIC_VECTOR(3 downto 0) := "1110";
    
    constant operation_bin_c : OPERATION_BIN_T := (
        READ_WIDTH1    => OP_READ_WIDTH1,
        READ_WIDTH2    => OP_READ_WIDTH2,
        READ_BLOCK_LEN => OP_READ_BLOCK_LEN,
        
        SET_ADDR    => OP_SET_ADDR,
        REG_READ    => OP_REG_READ,
        REG_WRITE   => OP_REG_WRITE,
        
        BLOCK_READ  => OP_BLOCK_READ
    );

    signal op_sr_l : STD_LOGIC_VECTOR(3 downto 0);
    signal operation_l : OPERATION_T;
    signal op_short_l : STD_LOGIC;

-- DATA REGISTER
    signal da_sr_extender_l : STD_LOGIC_VECTOR((LA_CFG_JTAG_MAX_BLOCK_C-1)*(LA_CFG_WB_DATA_WIDTH_C+1)-1 downto 0);
    signal da_sr_l : STD_LOGIC_VECTOR(DEFAULT_WIDTH_C- 1 downto 0);
    
    signal da_wb_address_l : STD_LOGIC_VECTOR(LA_CFG_WB_ADDR_WIDTH_C- 1 downto 0) := (others => '0');
    signal da_wb_write_buffer_l : STD_LOGIC_VECTOR(LA_CFG_WB_DATA_WIDTH_C- 1 downto 0) := (others => '0');

    signal da_trans_words_l : INTEGER range 0 to LA_CFG_JTAG_MAX_BLOCK_C := 0;
    signal da_trans_bits_l  : INTEGER range 0 to max_f(LA_CFG_WB_DATA_WIDTH_C+1, LA_CFG_WB_ADDR_WIDTH_C+1) := 0;
        
-- WISHBONE INTERFACE
    type WB_FSM_STATES_T is (WB_FSM_IDLE, 
                             WB_FSM_READ_WAIT, WB_FSM_READ_LATCH,
                             WB_FSM_WRITE_WAIT, WB_FSM_WRITE_INC,
                             WB_FSM_LATCH_ADDRESS, WB_FSM_INC_ADDRESS);
                             
    type WB_REQUEST_T is record
        read_op,
        read_data,
        write_data,
        latch_address,
        inc_address1,   -- need two different signals, as requests from different time domains
        inc_address2    : UNSIGNED(2 downto 0);
    end record;
    
    type WB_FSM_T is record
        state : WB_FSM_STATES_T;
        
        requests : WB_REQUEST_T;
            
        address : STD_LOGIC_VECTOR(LA_CFG_WB_ADDR_WIDTH_C-1 downto 0);
        wb_stb  : STD_LOGIC;
        wb_we   : STD_LOGIC;
    end record;
    
    constant wb_fsm_reset_c : WB_FSM_T := (WB_FSM_IDLE, (others => (others => '0')), (others=>'0'), '0','0');
    
    signal wb_fsm_l, wb_fsm_next_l : WB_FSM_T := wb_fsm_reset_c;
    signal wb_requests_l, wb_requests_reg_l : WB_REQUEST_T := (others => (others => '0'));
        
    signal wb_read_buffer_l : STD_LOGIC_VECTOR(LA_CFG_WB_DATA_WIDTH_C-1 downto 0);
begin
    assert LA_CFG_WB_DATA_WIDTH_C <= 255 and LA_CFG_WB_ADDR_WIDTH_C <= 255
        report "This wishbone master supports only LA_CFG_WB_DATA_WIDTH_C <= 255 and LA_CFG_WB_ADDR_WIDTH_C < 255"
        severity failure;
        
    assert LA_CFG_JTAG_MAX_BLOCK_C mod 4 = 0
        report "LA_CFG_JTAG_MAX_BLOCK_C must be a multiple of 4"
        severity error;
        

-- OPERATION REGISTER
    op_shift: process(jtag_state_i, jtag_op_reg_i) is begin
        if rising_edge(jtag_op_reg_i.clk) and jtag_op_reg_i.sel = '1' then
            if jtag_state_i.capture = '1' then
                case(operation_l) is
                    when READ_WIDTH1 => op_sr_l <= DEFAULT_WIDTH_REP_C(3 downto 0);
                    when READ_WIDTH2 => op_sr_l <= DEFAULT_WIDTH_REP_C(7 downto 4);
                    when READ_BLOCK_LEN => op_sr_l <= LENGTH_REP_C(5 downto 2);
                    when others =>      op_sr_l <= operation_bin_c(operation_l);
                end case;
            elsif jtag_state_i.shift = '1' and jtag_op_reg_i.sel = '1' then
                op_sr_l     <= jtag_op_reg_i.tdi & op_sr_l(op_sr_l'HIGH downto 1);
            end if;
        end if;
    end process;
    jtag_op_reg_tdo_o <= op_sr_l(0);
    
    op_latch: process(jtag_state_i, jtag_op_reg_i, op_sr_l) is
    begin
        if jtag_state_i.reset = '1' then
            operation_l <= READ_WIDTH1;
            op_short_l <= '0';
        elsif rising_edge(jtag_state_i.update) and jtag_op_reg_i.sel = '1' then
            case(op_sr_l) is
                when OP_READ_WIDTH1    => operation_l <= READ_WIDTH1; op_short_l <= '0';
                when OP_READ_WIDTH2    => operation_l <= READ_WIDTH2; op_short_l <= '1';
                when OP_READ_BLOCK_LEN => operation_l <= READ_BLOCK_LEN;
                
                when OP_SET_ADDR    => operation_l <= SET_ADDR;
                when OP_REG_WRITE   => operation_l <= REG_WRITE;
                when OP_REG_READ    =>
                    operation_l <= REG_READ;
                    wb_requests_l.read_op <= wb_requests_l.read_op + 1;
                    
                when OP_BLOCK_READ  =>
                    operation_l <= BLOCK_READ;
                    wb_requests_l.read_op <= wb_requests_l.read_op + 1;
                    
                when others => operation_l <= READ_WIDTH1;
            end case;
        end if;
    end process;
    
-- DATA REGISTER
    da_shift: process(jtag_data_reg_i, jtag_state_i) is
        variable sr_length_v : integer;
        variable read_parity_v : STD_LOGIC;
    begin
            if rising_edge(jtag_data_reg_i.clk) and jtag_data_reg_i.sel = '1' then
                if jtag_state_i.capture = '1' then
                -- default value of data register is the last value read from wishbone
                    da_sr_l(wb_read_buffer_l'RANGE) <= wb_read_buffer_l;
                    da_sr_l(da_sr_l'HIGH downto wb_read_buffer_l'LENGTH) <= (others => '0');

                    case(operation_l) is
                        when READ_WIDTH1 => da_sr_l <= STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_WB_DATA_WIDTH_C, da_sr_l'LENGTH));
                        when SET_ADDR    => da_sr_l <= STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_WB_ADDR_WIDTH_C, da_sr_l'LENGTH));
                        when others => NULL;
                    end case;

                -- initialize counter for block reading
                    da_trans_words_l <= LA_CFG_JTAG_MAX_BLOCK_C;
                    
                -- initialize counter for bits in one word
                    da_trans_bits_l <= da_sr_l'LENGTH;
                    if op_short_l='1' then
                        case(operation_l) is
                            when SET_ADDR    => da_trans_bits_l <= LA_CFG_WB_ADDR_WIDTH_C+1;
                            when REG_READ    => da_trans_bits_l <= LA_CFG_WB_DATA_WIDTH_C+1;
                            when others => NULL;
                        end case;
                    elsif operation_l = BLOCK_READ then
                        da_trans_bits_l <= LA_CFG_WB_DATA_WIDTH_C+1;
                    end if;
                    
                 -- use odd parity bit for every word
                    read_parity_v := '1';
                elsif jtag_state_i.shift = '1' then
                 -- huge shift register used when in block-reading mode; xst should use shift-register primitives
                    da_sr_extender_l <= jtag_data_reg_i.tdi & da_sr_extender_l(da_sr_extender_l'HIGH downto 1);
                    
                    
                    read_parity_v := read_parity_v xor da_sr_l(0);
                    
                    if operation_l = BLOCK_READ then
                        da_sr_l(LA_CFG_WB_DATA_WIDTH_C downto 0) <= da_sr_extender_l(0) & da_sr_l(LA_CFG_WB_DATA_WIDTH_C downto 1);
                        
                        if da_trans_bits_l = LA_CFG_WB_DATA_WIDTH_C then 
                            wb_requests_l.inc_address1 <= wb_requests_l.inc_address1 + 1;
                            da_trans_bits_l <= da_trans_bits_l - 1;
                        elsif da_trans_bits_l = LA_CFG_WB_DATA_WIDTH_C - 1 then
                            wb_requests_l.read_data <= wb_requests_l.read_data + 1;
                            da_trans_bits_l <= da_trans_bits_l - 1;
                        
                        elsif da_trans_bits_l = 2 then
                            da_sr_l(0) <= read_parity_v;
                            read_parity_v := '1';
                            
                        elsif da_trans_bits_l = 1 and da_trans_words_l /= 1 then
                            da_sr_l(wb_read_buffer_l'RANGE) <= wb_read_buffer_l;
                            da_trans_bits_l <= LA_CFG_WB_DATA_WIDTH_C;
                            da_trans_words_l <= da_trans_words_l - 1;
                        
                        elsif da_trans_bits_l /= 0 then
                            da_trans_bits_l <= da_trans_bits_l - 1;
                        end if;
                    else
                        sr_length_v := da_sr_l'LENGTH;
                        if op_short_l='1' then
                            case(operation_l) is
                                when SET_ADDR             => sr_length_v := LA_CFG_WB_ADDR_WIDTH_C+1;
                                when REG_WRITE | REG_READ => sr_length_v := LA_CFG_WB_DATA_WIDTH_C+1;
                                when others => NULL;
                            end case;
                        end if;

                        da_sr_l(sr_length_v-1 downto 0)     <= jtag_data_reg_i.tdi & da_sr_l(sr_length_v-1 downto 1);
                        
                        if da_trans_bits_l = 2 then
                            da_sr_l(0) <= read_parity_v;
                        end if;
                        
                        if da_trans_bits_l /= 1 then
                            da_trans_bits_l <= da_trans_bits_l - 1;                        
                        end if;
                    end if;
                end if;
            end if;
    end process;
    jtag_data_reg_tdo_o <= da_sr_l(0);
    
    da_latch: process(jtag_data_reg_i, jtag_state_i, operation_l, da_sr_l) is
        variable parity_v : STD_LOGIC;
    begin
        if jtag_state_i.reset = '1' then
            da_wb_address_l <= (others => '0');
        elsif rising_edge(jtag_state_i.update) and jtag_data_reg_i.sel = '1' then
            parity_v := '1';
            for i in 0 to DEFAULT_WIDTH_C-1 loop
                parity_v := parity_v xor da_sr_l(i);
            end loop;
                
            if parity_v='0' then
                case(operation_l) is
                    when SET_ADDR =>
                        da_wb_address_l <= da_sr_l(da_wb_address_l'RANGE);
                        wb_requests_l.latch_address <= wb_requests_l.latch_address + 1;

                    when REG_WRITE => 
                        da_wb_write_buffer_l <= da_sr_l(da_wb_write_buffer_l'RANGE);
                        wb_requests_l.write_data <= wb_requests_l.write_data + 1;

                    when REG_READ =>
                        wb_requests_l.inc_address2 <= wb_requests_l.inc_address2 + 1;
                    
                    when others => NULL;
                end case;
            end if;
        end if;
    end process;

-- WISHBONE INTERFACE
    process(wb_clk_i) is 
    begin
        if rising_edge(wb_clk_i) then
            wb_requests_reg_l <= wb_requests_l;
            if wb_rst_i = '1' then
                wb_fsm_l <= wb_fsm_reset_c;
                wb_fsm_l.requests <= wb_requests_reg_l;
            else
                if wb_fsm_next_l.state = WB_FSM_IDLE then
                    wb_o.data    <= da_wb_write_buffer_l;
                end if;
            
                if wb_fsm_next_l.state = WB_FSM_READ_LATCH then
                    wb_read_buffer_l <= wb_i.data;
                end if;
                
                wb_fsm_l <= wb_fsm_next_l;
            end if;
        end if;
    end process;

    wb_o.address <= wb_fsm_l.address;
    wb_o.stb <= wb_fsm_l.wb_stb;
    wb_o.we  <= wb_fsm_l.wb_we;

    process(wb_fsm_l, wb_i.ack, wb_fsm_l.requests, wb_requests_reg_l) is
    begin
        wb_fsm_next_l.state <= wb_fsm_l.state;
        case (wb_fsm_l.state) is
            when WB_FSM_IDLE =>
                if (wb_fsm_l.requests.latch_address /= wb_requests_reg_l.latch_address) then
                    wb_fsm_next_l.state <= WB_FSM_LATCH_ADDRESS;
                
                elsif (wb_fsm_l.requests.inc_address1 /= wb_requests_reg_l.inc_address1) or
                      (wb_fsm_l.requests.inc_address2 /= wb_requests_reg_l.inc_address2) then
                    wb_fsm_next_l.state <= WB_FSM_INC_ADDRESS;

                elsif (wb_fsm_l.requests.read_op /= wb_requests_reg_l.read_op) or
                      (wb_fsm_l.requests.read_data /= wb_requests_reg_l.read_data) then
                    wb_fsm_next_l.state <= WB_FSM_READ_WAIT;
                elsif (wb_fsm_l.requests.write_data /= wb_requests_reg_l.write_data) then
                    wb_fsm_next_l.state <= WB_FSM_WRITE_WAIT;
                end if;
            
            when WB_FSM_READ_WAIT =>
                if (wb_i.ack = '1') then
                    wb_fsm_next_l.state <= WB_FSM_READ_LATCH;
                end if;
            
            when WB_FSM_READ_LATCH =>
                wb_fsm_next_l.state <= WB_FSM_IDLE;
            
            when WB_FSM_WRITE_WAIT =>
                if (wb_i.ack = '1') then
                    wb_fsm_next_l.state <= WB_FSM_WRITE_INC;
                end if;
                
            when WB_FSM_WRITE_INC =>
                wb_fsm_next_l.state <= WB_FSM_IDLE;
                
            when WB_FSM_LATCH_ADDRESS | WB_FSM_INC_ADDRESS =>
                wb_fsm_next_l.state <= WB_FSM_IDLE;
        end case;
    end process;

    wb_fsm_next_l.wb_stb <= 
        '1' when wb_fsm_next_l.state = WB_FSM_READ_WAIT else
        '1' when wb_fsm_next_l.state = WB_FSM_READ_LATCH else
        '1' when wb_fsm_next_l.state = WB_FSM_WRITE_WAIT else
        '0';

    wb_fsm_next_l.wb_we  <= active_high_c(wb_fsm_next_l.state = WB_FSM_WRITE_WAIT);

-- requests
    
    wb_fsm_next_l.requests.read_op <=
        wb_requests_reg_l.read_op when wb_fsm_l.state = WB_FSM_READ_LATCH else
        wb_fsm_l.requests.read_op;
    wb_fsm_next_l.requests.read_data <=
        wb_requests_reg_l.read_data  when wb_fsm_l.state = WB_FSM_READ_LATCH else
        wb_fsm_l.requests.read_data;
    
    wb_fsm_next_l.requests.write_data <=
        wb_requests_reg_l.write_data when wb_fsm_l.state = WB_FSM_WRITE_INC else
        wb_fsm_l.requests.write_data;
        
    wb_fsm_next_l.requests.latch_address <=
        wb_requests_reg_l.latch_address when wb_fsm_l.state = WB_FSM_LATCH_ADDRESS else
        wb_fsm_l.requests.latch_address;
        
    wb_fsm_next_l.requests.inc_address1 <=
        wb_requests_reg_l.inc_address1 when wb_fsm_l.state = WB_FSM_INC_ADDRESS else
        wb_fsm_l.requests.inc_address1;        
    wb_fsm_next_l.requests.inc_address2 <=
        wb_requests_reg_l.inc_address2 when wb_fsm_l.state = WB_FSM_INC_ADDRESS else
        wb_fsm_l.requests.inc_address2;        

    wb_fsm_next_l.address <=
        da_wb_address_l when wb_fsm_l.state = WB_FSM_LATCH_ADDRESS else
        STD_LOGIC_VECTOR(UNSIGNED(wb_fsm_l.address) + TO_UNSIGNED(1, wb_fsm_l.address'LENGTH)) when wb_fsm_l.state = WB_FSM_INC_ADDRESS else
        STD_LOGIC_VECTOR(UNSIGNED(wb_fsm_l.address) + TO_UNSIGNED(1, wb_fsm_l.address'LENGTH)) when wb_fsm_l.state = WB_FSM_WRITE_INC else
        wb_fsm_l.address;
end architecture;