--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   11.09.2011
--
-- Description:
--   This module performs a run-length-encoding. If an input does not change,
--   it is not written again to memory, but a counter is incremented, counting
--   how many cycles the input remains unchanged. The counter value is written
--   to memory just before it overflows or the signal changes. Counter values
--   are stored with the meta-flag = 1. The counter is [LA_CFG_BANK_WIDTH_C]
--   bits width.
--
--   This implementation does not handle ISR well. Even if just one bank changes,
--   the whole set of active banks is stored. 
-- 
--   A trigger event is delayed, in order to arrive with the first bank
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

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    
entity RUN_LENGTH_ENCODER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_OUT_T;
        stage_o     : out ENC_STAGE_OUT_T
    );
end RUN_LENGTH_ENCODER;

architecture behav of RUN_LENGTH_ENCODER is
-- FSM
    constant max_counter_c  : STD_LOGIC_VECTOR(stage_i.data'RANGE) := (others => '1');
    constant null_counter_c : STD_LOGIC_VECTOR(stage_i.data'RANGE) := (others => '0');

    type FSM_STATE_T is (FSM_INIT, FSM_STORE_CYCLE_INIT, FSM_STORE_CYCLE,
    				     FSM_COUNTER_INIT, FSM_COUNTER_INC, FSM_COUNTER_IDLE,
    				     FSM_STORE_COUNTER_AND_DATA);
    type FSM_T is record
        state : FSM_STATE_T;
        trigger : STD_LOGIC;
        load_counter : STD_LOGIC;
        load_data : STD_LOGIC;
        counter : STD_LOGIC_VECTOR(stage_i.data'RANGE);
    end record;
    signal fsm_l : FSM_T :=  (FSM_INIT, '0', '0', '0', null_counter_c);
    signal fsm_next_l : FSM_T;


-- Data Path (shift register)
    signal sr_counter_l, sr_data_l, sr_out_l : STD_LOGIC_VECTOR(stage_i.data'HIGH+2 downto 0);
begin
    assert not (LA_CFG_RLE_C and LA_CFG_MUX_METHOD_C = ISR)
        report "Run length encoding does not fully support the input shift register. If just one bank changed all are stored."
        severity warning;

-- Synchronisation
    proc_sync: process(sa_clk_i) is
   		variable trigger_delay_v : STD_LOGIC := '0';
    begin
        if rising_edge(sa_clk_i) then
            if stage_i.reset='1' then
                fsm_l <= (FSM_INIT, '0', '0', '0', null_counter_c);
            else
                fsm_l <= fsm_next_l;
            end if;

            stage_o.reset <= stage_i.reset;		  -- delay by one, just like data
            
            -- shift register data
            sr_counter_l <= "10" & fsm_next_l.counter;
            sr_data_l    <= "0"  & fsm_next_l.trigger & stage_i.data;
        end if;
    end process;

-- FSM transitions and output functions
    proc_next_state: process(stage_i, fsm_l) is
        variable changed : BOOLEAN;
    begin
		changed := stage_i.changed = '1' or (fsm_l.trigger = '0' and stage_i.trigger = '1');
        
        fsm_next_l.state <= fsm_l.state; -- default: stay in same state
        case(fsm_l.state) is
            when FSM_INIT =>
                fsm_next_l.state <= FSM_STORE_CYCLE_INIT;
        
            when FSM_STORE_CYCLE_INIT | FSM_STORE_CYCLE =>
            	if stage_i.new_cycle='1' then
					if changed then
						fsm_next_l.state <= FSM_STORE_CYCLE_INIT;
					else
						fsm_next_l.state <= FSM_COUNTER_INC;
					end if;
            	else
            		fsm_next_l.state <= FSM_STORE_CYCLE;
            	end if;
            
            when FSM_COUNTER_INIT | FSM_COUNTER_INC | FSM_COUNTER_IDLE =>
            	if stage_i.new_cycle='1' then
					if changed then
						fsm_next_l.state <= FSM_STORE_COUNTER_AND_DATA;
					else
						fsm_next_l.state <= FSM_COUNTER_INC;
					end if;
            	else
            		fsm_next_l.state <= FSM_COUNTER_IDLE;
            	end if;
            	
           	when FSM_STORE_COUNTER_AND_DATA =>
            	if stage_i.new_cycle='1' then
					if changed then
						fsm_next_l.state <= FSM_STORE_CYCLE_INIT;
					else
						fsm_next_l.state <= FSM_COUNTER_INIT;
					end if;
            	else
					fsm_next_l.state <= FSM_STORE_CYCLE;
            	end if;
        end case;
    end process;
    
    fsm_next_l.trigger <= 
    	stage_i.trigger or fsm_l.trigger when fsm_next_l.state = FSM_STORE_CYCLE_INIT else
    	stage_i.trigger or fsm_l.trigger when fsm_next_l.state = FSM_STORE_COUNTER_AND_DATA else
    	fsm_l.trigger;
    
    fsm_next_l.load_counter <= active_high_c(fsm_next_l.state = FSM_STORE_COUNTER_AND_DATA);
    
    fsm_next_l.load_data <= 
        '1' when fsm_next_l.state = FSM_STORE_CYCLE_INIT else
        '1' when fsm_next_l.state = FSM_STORE_CYCLE else
        '1' when fsm_next_l.state = FSM_STORE_COUNTER_AND_DATA else
        '0';
        
    fsm_next_l.counter <= 
        null_counter_c when fsm_next_l.state = FSM_STORE_CYCLE_INIT else
        STD_LOGIC_VECTOR(TO_UNSIGNED(1, fsm_l.counter'LENGTH)) when fsm_next_l.state = FSM_COUNTER_INIT else
        STD_LOGIC_VECTOR(UNSIGNED(fsm_l.counter) + TO_UNSIGNED(1, fsm_l.counter'LENGTH)) when fsm_next_l.state = FSM_COUNTER_INC else
        fsm_l.counter;

-- SHIFT REGISTER    
    sr : dual_input_sr 
    generic map (LA_CFG_BANK_WIDTH_C+2)
    port map (
        clk_i => sa_clk_i,
        reset_i => stage_i.reset,
        
        input1_i => sr_counter_l,
        input2_i => sr_data_l,
        
        ena1_i   => fsm_l.load_counter,
        ena2_i   => fsm_l.load_data,

        output_o => sr_out_l,
        valid_o  => stage_o.store
    );
    
    stage_o.data    <= sr_out_l(stage_o.data'RANGE);
    stage_o.trigger <= sr_out_l(stage_o.data'HIGH+1);
    stage_o.meta    <= sr_out_l(stage_o.data'HIGH+2);
end behav;