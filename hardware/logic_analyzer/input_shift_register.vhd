---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   10.08.2011
--
-- Description:
--   This entity is the first stage in the sampling pipeline. It latches
--   all input signals at the same time and then outputs all active banks
--   serially (synchronously to [sa_clk_i]). 
--
--   The input signal [bank_ena_i] functions as a bit mask. The LSB represents
--   the first bank, while more significant bits are associated with higher banks.
--   When shifting data banks that are not enabled (bank_ena_i(j) = '0') are skipped.
--
--   At least one bank has to be enabled. Thus the only illegal value for [bank_ena_i] is
--   zero.
--
--   A shifting cycle starts with the least active bank and ends with the most significant
--   active bank. A trigger event is delayed until a new shifting cycle starts.
--
--   The changed flag is asserted directly after reset or if one of the enabled banks 
--   changes.
--
--   If a trigger event occures within a shift cycle it is delayed until a new cycle starts.
--   Once a trigger is asserted it is kept until reset.
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
    use IEEE.NUMERIC_STD.ALL;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;

entity INPUT_SHIFT_REGISTER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_IN_T;
        stage_o     : out MUX_STAGE_OUT_T;
        
    -- Config
        bank_ena_i  : in STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C - 1 downto 0)
    );
end INPUT_SHIFT_REGISTER;

architecture RTL of INPUT_SHIFT_REGISTER is
    constant EMPTY_BANK_MASK_C : STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C-1 downto 0) := (others => '0');

    type STATE_T is record
        bank_mask   : STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C-1 downto 0);
        bank_index  : INTEGER RANGE 0 to LA_CFG_BANK_COUNT_C-1;
    end record;
    
    signal state_l, next_state_l : STATE_T := (EMPTY_BANK_MASK_C, 0);
    signal data_l : STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C*LA_CFG_BANK_WIDTH_C-1 downto 0);
    
    signal stage_out_l : MUX_STAGE_OUT_T;
    
    signal changed_l : STD_LOGIC;
begin
    proc_load: process(sa_clk_i) is
    begin
        if rising_edge(sa_clk_i) then
            stage_out_l.reset <= stage_i.reset;

            if (stage_i.reset = '1') or (next_state_l.bank_mask = EMPTY_BANK_MASK_C) then
                -- load new data and begin new sampling cycle
                data_l <= stage_i.data;
                state_l <= (bank_ena_i, 0);
                
                stage_out_l.new_cycle <= '1';
                stage_out_l.trigger <= stage_i.trigger;
                stage_out_l.changed <= changed_l;
            else
                state_l <= next_state_l;
                stage_out_l.new_cycle <= '0';
            end if;
        end if;
    end process;

    proc_2nd_stage: process(sa_clk_i) is
    begin
        if rising_edge(sa_clk_i) then
            stage_o <= stage_out_l;
        end if;
    end process;
    
-- combinatorial logic
    -- data multiplexer
    stage_out_l.data <= data_l( (next_state_l.bank_index+1)*LA_CFG_BANK_WIDTH_C - 1 downto next_state_l.bank_index*LA_CFG_BANK_WIDTH_C );
    
    -- shift to next active bank
    process(state_l) is
    begin
        next_state_l <= (EMPTY_BANK_MASK_C, 0); -- prevent latches
    
        for i in 1 to LA_CFG_BANK_COUNT_C - 1 loop
            if state_l.bank_mask(i) = '1' then
                next_state_l.bank_mask(next_state_l.bank_mask'HIGH - i - 1 downto 0) <= 
                    state_l.bank_mask(state_l.bank_mask'HIGH downto i);
                next_state_l.bank_mask(next_state_l.bank_mask'HIGH downto next_state_l.bank_mask'HIGH - i) <=
                    (others => '0');
                
                next_state_l.bank_index <= next_state_l.bank_index+i;
                exit;
            end if;
        end loop;
    end process;
    
    -- change bit
    process(data_l, stage_i.data, bank_ena_i) is
        variable changed_v : BOOLEAN;
    begin
        changed_v := FALSE;
        
        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
            changed_v := changed_v or (bank_ena_i(i) = '1' and 
                data_l( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i*LA_CFG_BANK_WIDTH_C) 
                /= stage_i.data( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i*LA_CFG_BANK_WIDTH_C) );
        end loop;
    
        changed_l <= active_high_c(changed_v);
    end process;
end RTL;