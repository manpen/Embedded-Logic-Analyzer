--------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   15.09.2011
--
-- Description:
--   This module handels the write access to the memory. The memory is to be connected
--   to this entity externally. It must be garantueed that any value provided on mem_o
--   with mem_o.we = 1 is handled with the next risign edge of sa_clk_i. However that
--   applies only if this module is not in the [state_o.state] = FULL state. At this
--   time the memory can savely be disconnected. This allows for a single-port memory,
--   that - of course - can only be read once writing is finished.
--
--   A ram-based approach (in constrast to an fifo) of an ring-buffer is implemented.
--   While untriggered every active (store=1) input is stored from the lowest mem-address
--   asceding. If the highest address is reached, it overflows and begins overwritten 
--   the old data starting from the address 0.
--
--   If the trigger-events arrives a stop address is calculted in order to protect
--   the last [base_offset_i] values from begin overwritten. If there are less values
--   stored in mem, the limit is chosen to protect only those. When the write-address
--   reaches that limit, [state_o.state] changes from CAPTURING to FULL and this unit
--   become inactive. Recovery from this state is possible via a pipeline reset.
--
--   The stop address is outputted on [state_o.base_address]. The address that was
--   active when the trigger event arrived is accessible via [state_o.trigger_address].
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
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;

entity MEMORY_MANAGER is
    port (
        sa_clk_i      : in STD_LOGIC;
        stage_i       : in ENC_STAGE_OUT_T;
        base_offset_i : in STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
        
        mem_o       : out MEM_MOSI_T;
        state_o     : out MEM_STATE_OUT_T
    );
end MEMORY_MANAGER;

architecture Behavioral of MEMORY_MANAGER is
-- write address
    signal overflow_l : STD_LOGIC := '0';  -- indicates whether ring puffer is currently
                                           -- overwriting old values
    signal write_address_l : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0) := (others => '0');
    
-- limit
    signal state_l : MEM_STATE_T;
    signal stop_address_l  : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
begin
    process(sa_clk_i) is
        variable new_overflow_v : STD_LOGIC;
        variable new_write_address_v : STD_LOGIC_VECTOR(write_address_l'RANGE);
    begin
        if rising_edge(sa_clk_i) then
            state_o.reset <= stage_i.reset;
            
            if stage_i.reset = '1' then
                state_l <= EMPTY;
                write_address_l <= (others => '0');
                overflow_l <= '0';
            else
            -- Calculate new write address
                if write_address_l = STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_MEM_MAX_ADDR_C, write_address_l'LENGTH)) then
                    new_write_address_v := (others => '0');
                    new_overflow_v := '1';
                else
                    new_write_address_v := STD_LOGIC_VECTOR(UNSIGNED(write_address_l) + TO_UNSIGNED(1, write_address_l'LENGTH));
                    new_overflow_v := overflow_l;
                end if;
            
            -- FSM
                if state_l = EMPTY and stage_i.trigger = '1' and stage_i.store = '1' then
                    -- start capturing, i.e. calculate limits
                    if write_address_l >= base_offset_i then
                        stop_address_l <= STD_LOGIC_VECTOR(UNSIGNED(write_address_l) - UNSIGNED(base_offset_i));
                    elsif overflow_l = '1' then
                        stop_address_l <= STD_LOGIC_VECTOR(UNSIGNED(write_address_l) - UNSIGNED(base_offset_i) + LA_CFG_MEM_MAX_ADDR_C);
                    else
                        stop_address_l <= (others => '0');
                    end if;
                    
                    state_l <= CAPTURING;
                    state_o.trigger_address <= write_address_l;
                elsif state_l = CAPTURING and new_write_address_v = stop_address_l then
                    state_l <= FULL;
                end if;
            
            -- Update write address
                if stage_i.store = '1' and state_l /= FULL then
                    write_address_l <= new_write_address_v;
                    overflow_l <= new_overflow_v;
                end if;
            end if;
        end if;
    end process;

-- MEMORY INTERFACE    
    mem_o.address <= write_address_l;
    mem_o.we <= stage_i.store and active_high_c(state_l /= FULL);
    
    mem_o.data(stage_i.data'RANGE) <= stage_i.data;
    mem_o.data(mem_o.data'HIGH downto stage_i.data'LENGTH) <= (others => stage_i.meta);

    
-- STATE
    state_o.state <= state_l;
    state_o.base_address <= stop_address_l;
end Behavioral;

