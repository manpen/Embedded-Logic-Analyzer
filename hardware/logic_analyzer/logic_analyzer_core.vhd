---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   09.09.2011
--
-- Description:
--   This entity is the top module of the logic analyzer. It is the only module in the 
--   LOGIC_ANALYZER-library that should be directly access by externals modules.
-- 
--   It instantiates the sampling pipeline according to the settings in the LOGIC_ANALYZER.CONFIG_PKG
--   package and offers access to all setting via flexible wishbone interface. The address space is
--   seperated into four logical ranges:
--     0x00 to 0x0F     Information about configuration
--     0x10 to  A       Setup of all values except triggers
--       A  to  B       Trigger setup
--       C  to  D       Memory access
--
--   While the writing to the sampling memory is implemented via a custom interface with strict timing limits,
--   reading is done via a wishbone interface that allows for an arbitrary delay. Hence it is possible to build
--   the external memery based on a single port memory. The mem is mapped into the wishbone address space.
--   All requests with the most signifcant address bit set are routed to the memory interface, the lower
--   half of the space is dealt with internally.
--   
--   It is garantueed that all read and write operations within the lower half of the address space handled
--   on the next raising edge of [wb_clk_i].
--
-- Wishbone-Datasheet:
--   Supported Cycles: 
--      Slave, SINGLE READ/WRITE, RMW
--   Data port, size:   [LA_CFG_WB_DATA_WIDTH_C]-bit
--   Data port, granularity: [LA_CFG_WB_DATA_WIDTH_C]-bit
--   Data port, maximaum operend size: [LA_CFG_WB_DATA_WIDTH_C]-bit
--   Data transfer ordering: big-endian
--   Data transfer sequencing: undefined
--   Signal list:
--      Signal name     Wishbone Equiv
--      wb_clk_i        CLK_I
--      wb_rst_i        RST_I
--      wb_i.stb        STB_I
--      wb_o.ack        ACK_O
--      wb_i.we         WE_I
--      wb_i.addr       ADR_I()
--      wb_i.data       DAT_I()
--      wb_o.data       DAT_O()
--
--   Address scheme:
--    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--    !! WHEN SIMULATING/SYNTHESIZING THIS MODULE A TABLE OF THE ADDRESSES OF THE 
--    !! MOVING REGISTERS IS PRINTED OUT. SEE CONSOLE.
--    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
--
--      FROM        | SIZE          | In    | Out   | MAPPING
--      ------------|---------------|-------|------------------------------------------------
--      0x00        | 1             |       | Out   | Interface Version (=2)
--      0x01        | 1             |       | Out   | Minor Revision (=1)
--      0x02        | 1             |       | Out   | Wishbone-Interface Data Width
--      0x03        | 1             |       | Out   | Bank Width
--      0x04        | 1             |       | Out   | Bank Count
--      0x05        | 1             |       | Out   | Memory Address Width
--      0x06        | 1             |       | Out   | Memory Data Width
--      0x07        | 1             |       | Out   | Clock Divider Width
--      0x08        | 1             |       | Out   | Trigger Input Width
--      0x09        | 1             |       | Out   | Edge Trigger Count
--      0x0A        | 1             |       | Out   | Value Trigger Count
--      0x0B        | 1             |       | Out   | Capability Information
--      .           |               |       |       |          0. and 1. Bit => Input Multiplexer Method
--      .           |               |       |       |                           (-> [LA_CFG_MUX_METHOD_C])
--      .           |               |       |       |                           (00: MUX, 01: ISR, 10: reserved, 11: reserved)
--      .           |               |       |       |                 2. Bit => LA_CFG_NO_ESSENTIAL_WB_READ_C 
--      .           |               |       |       |                 3. Bit => LA_CFG_READ_CURRENT_DATA 
--      .           |               |       |       |                 4. Bit => LA_CFG_TEST_PATTERN_C 
--      .           |               |       |       |                 5. Bit => reserved 
--      .           |               |       |       |                 6. Bit => LA_CFG_RLE_C 
--      0x0C        | 1             |       |       |  unused
--      0x0D        | 1             |       |       |  unused
--      0x0E        | 1             |       |       |  unused
--      0x10        | 1             | In    | Out   | Status Register 
--      .           |               |       | Out   |                 0-1Bit => Memory Mgr State
--      .           |               |       | Out   |                           (00: reserved, 01: Empty, 10: Capturing, 11: Full)
--      .           |               |       |       |                
--      .           |               |       |       |                
--      .           |               | In    | Out   |                 2. Bit => unused
--      .           |               | In    | Out   |                 3. Bit => Run-Length-Encoding Enable
--      .           |               | In    | Out   |                 4-5Bit => Test pattern select
--      .           |               | In    |       |                 6. Bit => Soft Reset (to empty Mem Mgr)
--      .           |               | In    |       |                 7. Bit => Cold Reset (Reset all setup value)
--      ------------|---------------|-------|------------------------------------------------
--      0x11        | R(MuxInput)   | In    | Out?  | Input Select (Size and meaning depends on [LA_CFG_MUX_METHOD_C])
--      +           | R(ClkCntWdt)  | In    | Out?  | Clock divider: Treshold
--      +           | R(MemWidth)   |       | Out   | Memory Max Address
--      +           | R(MemWidth)   | In    | Out?  | Memory Base Offset
--      +           | R(MemWidth)   |       | Out   | Memory Base Address
--      +           | R(MemWidth)   |       | Out   | Memory Trigger Address
--      +           | R(MemWidth)   |       | Out   | Memory Current Write Address
--      +           | R(DataWidth)  |       | Out   | Current Data Sampled
--      +           | TriggerSize   | In    | Out?  | Trigger Wishbone Interface (see trigger.vhd for details)
--
--    Notation:
--      FROM column: "." means same address as row above, "+" means address plus the size above
--      SIZE column: R(x): Registers Count, i.e. ceil(x / wb_data_width)
--      DIR  column: "Out"  means write from entity   to wishbone
--                   "Out?" that read this register is not regarded essential,
--                          hence it is supported only if [LA_CFG_NO_ESSENTIAL_WB_READ_C] = true    
--                   "In"   means read  from wishbone to entity
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
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    use LOGIC_ANALYZER.WISHBONE.ALL;
    use LOGIC_ANALYZER.TXT_UTIL.ALL;
    
library unisim;
    use unisim.vcomponents.all;    

entity LOGIC_ANALYZER_CORE is
    port (
        -- input to sample from
        sa_clk_i  : in STD_LOGIC;
        sa_data_i : in STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        
        trg_data_i: in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trg_o : out STD_LOGIC;
        
        -- memory interface
        mem_state_o : out MEM_STATE_T;
        mem_clk_o : out STD_LOGIC;
        mem_o     : out MEM_MOSI_T;
        mem_wb_i  : in  WB_MEM_MISO_T;
        mem_wb_o  : out WB_MEM_MOSI_T := ((others => '0'), '0');
        
        -- wishbone interface
        wb_clk_i  : in  STD_LOGIC;
        wb_rst_i  : in  STD_LOGIC := '0';
        wb_o      : out WB_MISO_T := ((others => '0'), '0');
        wb_i      : in  WB_MOSI_T
    );
end LOGIC_ANALYZER_CORE;

architecture Behavioral of LOGIC_ANALYZER_CORE is
-- WISHBONE
    constant interface_version_c : STD_LOGIC_VECTOR(3 downto 0) := X"2";
    constant minor_hw_version_c : STD_LOGIC_VECTOR(3 downto 0) := X"2";
    type INPUT_MUX_REPR_T is array(INPUT_MUX_T) of STD_LOGIC_VECTOR(1 downto 0);
    constant wbi_input_mux_repr_c : INPUT_MUX_REPR_T := (MUX => "00", ISR => "01");
    
    type MEM_MGR_STATE_REPR_T is array(MEM_STATE_T) of STD_LOGIC_VECTOR(1 downto 0);
    constant mem_mgr_state_repr_c : MEM_MGR_STATE_REPR_T :=
        (EMPTY => "01", CAPTURING => "10", FULL => "11");
    
    constant WBI_CAPABILITIES_MASK_C : STD_LOGIC_VECTOR(7 downto 0) := 
        "0" &                                           -- bit 7: reserved
        active_high_c(LA_CFG_RLE_C) &                   -- bit 6 
        "0" &                                           -- bit 5: reserved
        active_high_c(LA_CFG_TEST_PATTERN_C) &          -- bit 4 
        active_high_c(LA_CFG_READ_CURRENT_DATA) &       -- bit 3 
        active_high_c(LA_CFG_NO_ESSENTIAL_WB_READ_C) &  -- bit 2
        wbi_input_mux_repr_c(LA_CFG_MUX_METHOD_C);      -- bits 1,0
                                                                     

    signal wbi_status_l : STD_LOGIC_VECTOR(7 downto 0);

    signal sa_data_l : STD_LOGIC_VECTOR(sa_data_i'RANGE);
    signal trg_data_l : STD_LOGIC_VECTOR(trg_data_i'RANGE);

-- clock divider signals
    signal sa_clkdiv_l, sa_clkdiv_unbuffered_l : STD_LOGIC;
    signal clk_threshold_l : STD_LOGIC_VECTOR(max_f(0, LA_CFG_CLKDIV_WIDTH_C-1) downto 0) := (others => '0');

-- test pattern generation signals   
    alias  test_pattern_select_l : STD_LOGIC_VECTOR(1 downto 0) is wbi_status_l(5 downto 4);
    signal test_sa_data_l  : STD_LOGIC_VECTOR(sa_data_i'RANGE);
    signal test_trg_data_l : STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
    
-- trigger signals
    signal trg_trigger_l : STD_LOGIC;
    signal trg_reset_l : STD_LOGIC;
    signal trg_wb_l : WB_MISO_T;
    
-- mux stage
    signal stg_mux_in_l : MUX_STAGE_IN_T;
    signal stg_mux_out_l : MUX_STAGE_OUT_T;
    
    signal isr_bank_ena_l : STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C - 1 downto 0) := (others => '1');
    signal mux_bank_select_l : STD_LOGIC_VECTOR(addr_width_f(LA_CFG_BANK_COUNT_C) - 1 downto 0) := (others => '0');

-- encodings
    alias  rle_enable_l : STD_LOGIC is wbi_status_l(3);
    signal rle_out_l : ENC_STAGE_OUT_T;
    
    signal enc_out_l : ENC_STAGE_OUT_T;

-- memory manager
    signal mem_offset_l : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0) := STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_MEM_MAX_ADDR_C / 2, LA_CFG_MEM_ADDR_WIDTH_C));
    signal mem_state_l  : MEM_STATE_OUT_T;
    signal mem_l : MEM_MOSI_T;
    
--  misc
    signal reset_l : STD_LOGIC;
begin
-- plausibility tests on configuration
    assert LA_CFG_WB_ADDR_WIDTH_C > LA_CFG_MEM_ADDR_WIDTH_C
        report "[LA_CFG_WB_ADDR_WIDTH_C] has to be bigger than [LA_CFG_MEM_ADDR_WIDTH_C] in order to address whole memory and la registes"
        severity failure;
        
    assert LA_CFG_MEM_DATA_WIDTH_C > LA_CFG_BANK_WIDTH_C or not LA_CFG_RLE_C
        report "With run length encoding activated [LA_CFG_MEM_DATA_WIDTH_C] has to be at least [LA_CFG_BANK_WIDTH_C] + 1"
        severity failure;

    assert LA_CFG_WB_DATA_WIDTH_C >= LA_CFG_MEM_DATA_WIDTH_C
        report "[LA_CFG_WB_DATA_WIDTH_C] has to be at least as high as [LA_CFG_MEM_DATA_WIDTH_C]"
        severity failure;
    
    assert LA_CFG_MEM_DATA_WIDTH_C >= LA_CFG_BANK_WIDTH_C
        report "[LA_CFG_MEM_DATA_WIDTH_C] has to be at least [LA_CFG_BANK_WIDTH_C]"
        severity failure;
        
-- WBI ADDRESS LAYOUT
    assert false report "WBI-ADDRESS SCHEME" severity note;
    assert false report "  LA_WBI_ADDR_status_c.............. 0x" & str(LA_WBI_ADDR_status_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_mux_c                  0x" & str(LA_WBI_ADDR_mux_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_clk_threshold_c....... 0x" & str(LA_WBI_ADDR_clk_threshold_c, 16) severity note; 
    assert false report "  LA_WBI_ADDR_mem_max_address_c      0x" & str(LA_WBI_ADDR_mem_max_address_c, 16) severity note; 
    assert false report "  LA_WBI_ADDR_mem_base_offset_c      0x" & str(LA_WBI_ADDR_mem_base_offset_c, 16) severity note; 
    assert false report "  LA_WBI_ADDR_mem_base_address_c.... 0x" & str(LA_WBI_ADDR_mem_base_address_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_mem_trigger_address_c  0x" & str(LA_WBI_ADDR_mem_trigger_address_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_mem_current_address_c. 0x" & str(LA_WBI_ADDR_mem_current_address_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_current_data_c         0x" & str(LA_WBI_ADDR_current_data_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_trigger_base_c........ 0x" & str(LA_WBI_ADDR_trigger_base_c, 16) severity note;
    assert false report "  LA_WBI_ADDR_max_c                  0x" & str(LA_WBI_ADDR_max_c, 16) severity note;
        
-- WISHBONE Interface
    process(wb_clk_i) is
        variable status_v : STD_LOGIC_VECTOR(7 downto 0);
        
        variable addr_base_v, addr_trigger_v, addr_current_v : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
    begin
        if rising_edge(wb_clk_i) then
            if wb_rst_i = '1' or wbi_status_l(7) = '1' then
                -- hard reset: clear all data
                status_v := (others => '0');
                mux_bank_select_l <= (others => '0');
                isr_bank_ena_l <= (others => '1');
                clk_threshold_l <= (others => '0');
                mem_offset_l <= STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_MEM_MAX_ADDR_C / 2, LA_CFG_MEM_ADDR_WIDTH_C));
            else
                if wb_i.address(LA_CFG_MEM_ADDR_WIDTH_C) = '1' then 
                    wb_o.data <= mem_wb_i.data;
                else
                    wb_o.data <= trg_wb_l.data;
                end if;            
            
            -- CONFIGURATION READ OUT
                wb_assign_read_constant_p(16#00#, interface_version_c, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#01#, minor_hw_version_c, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#02#, LA_CFG_WB_DATA_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#03#, LA_CFG_BANK_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#04#, LA_CFG_BANK_COUNT_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#05#, LA_CFG_MEM_ADDR_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#06#, LA_CFG_MEM_DATA_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#07#, LA_CFG_CLKDIV_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#08#, LA_CFG_TRG_WIDTH_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#09#, LA_CFG_EDGE_TRIGGERS_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#0A#, LA_CFG_VALUE_TRIGGERS_C, wb_i, wb_o.data);
                wb_assign_read_constant_p(16#0B#, WBI_CAPABILITIES_MASK_C, wb_i, wb_o.data);
                -- 0x0b: reserved
                -- 0x0c: reserved
                -- 0x0d: reserved
                -- 0x0e: reserved
                -- 0x0f: reserved

                status_v := "00" & wbi_status_l(5 downto 0);
                wb_assign_rw_variable_p(LA_WBI_ADDR_status_c, status_v, wb_i, wb_o.data);
                
                if LA_CFG_MUX_METHOD_C = MUX then
                    wb_assign_write_register_p(LA_WBI_ADDR_mux_c, mux_bank_select_l, wb_i);
                else
                    wb_assign_write_register_p(LA_WBI_ADDR_mux_c, isr_bank_ena_l, wb_i);                
                end if;
                    
                wb_assign_write_register_p(LA_WBI_ADDR_clk_threshold_c, clk_threshold_l, wb_i);
                wb_assign_write_register_p(LA_WBI_ADDR_mem_base_offset_c, mem_offset_l, wb_i);

                wb_assign_read_constant_p(LA_WBI_ADDR_mem_max_address_c, LA_CFG_MEM_MAX_ADDR_C, wb_i, wb_o.data);
                wb_assign_read_register_p(LA_WBI_ADDR_mem_base_address_c, mem_state_l.base_address, wb_i, wb_o.data);
                wb_assign_read_register_p(LA_WBI_ADDR_mem_trigger_address_c, mem_state_l.trigger_address, wb_i, wb_o.data);
                wb_assign_read_register_p(LA_WBI_ADDR_mem_current_address_c, mem_l.address, wb_i, wb_o.data);
                
                if LA_CFG_READ_CURRENT_DATA then
                    wb_assign_read_register_p(LA_WBI_ADDR_current_data_c, test_sa_data_l, wb_i, wb_o.data);
                end if;
                
                if LA_CFG_NO_ESSENTIAL_WB_READ_C then
                    if LA_CFG_MUX_METHOD_C = MUX then
                        wb_assign_read_register_p(LA_WBI_ADDR_mux_c, mux_bank_select_l, wb_i, wb_o.data);
                    else
                        wb_assign_read_register_p(LA_WBI_ADDR_mux_c, isr_bank_ena_l, wb_i, wb_o.data);
                    end if;
                    
                    if LA_CFG_CLKDIV_WIDTH_C > 0 then
                        wb_assign_read_register_p(LA_WBI_ADDR_clk_threshold_c, clk_threshold_l, wb_i, wb_o.data);
                    end if;
                    wb_assign_read_register_p(LA_WBI_ADDR_mem_base_offset_c, mem_offset_l, wb_i, wb_o.data);
                end if;
            end if;
            
            wbi_status_l(7 downto 2) <= status_v(7 downto 2);
        end if;
    end process;

    wb_o.ack <= wb_i.stb when wb_i.address(LA_CFG_MEM_ADDR_WIDTH_C) = '0' else mem_wb_i.ack;
    
    mem_wb_o.stb <= wb_i.stb and wb_i.address(wb_i.address'HIGH);
    mem_wb_o.address <= wb_i.address(mem_wb_o.address'RANGE);
    
    -- send reset through pipeline and hold it until it has propageded through
    -- the pipeline
    pipeline_reset : process(wb_clk_i) is
    begin
        if rising_edge(wb_clk_i) then
            if wbi_status_l(7 downto 6) /= "00" or wb_rst_i ='1' then
                reset_l <= '1';
            elsif mem_state_l.reset = '1' and enc_out_l.trigger = '0' then
                reset_l <= '0';
            end if;
        end if;
    end process;
    
-- First Pipeline Stage
    -- CLOCK DIVIDER
        gen_clkdiv: if LA_CFG_CLKDIV_WIDTH_C > 0 generate
            clkdiv : CLOCK_DIVIDER
            port map (
                sa_clk_i => sa_clk_i,
                sa_clk_o => sa_clkdiv_unbuffered_l,
                threshold_i => clk_threshold_l
            );

            clkdiv_buf : BUFG
            port map (
                O => sa_clkdiv_l,
                I => sa_clkdiv_unbuffered_l
            );
            
        end generate;
        
        noclkdiv: if LA_CFG_CLKDIV_WIDTH_C = 0 generate
            sa_clkdiv_l <= sa_clk_i;
        end generate;
        
        inp_sync: process(sa_clk_i, sa_data_i, trg_data_i) is
        begin
            if rising_edge(sa_clk_i) or (not LA_CFG_INPUT_SYNC_WITH_EXT_CLOCK) then
                sa_data_l <= sa_data_i;
                trg_data_l <= trg_data_i;
            end if;
        end process;
    
    -- TEST PATTERN GENERATION
        gen_test_pattern: if LA_CFG_TEST_PATTERN_C generate
            test_pattern : TEST_PATTERN_GEN
            port map (
                sa_clk_i => sa_clk_i,
                sa_data_i => sa_data_l,
                sa_data_o => test_sa_data_l,
                
                trg_data_i => trg_data_l,
                trg_data_o => test_trg_data_l,
                pattern_select_i => test_pattern_select_l
            );
        end generate;
       
        no_test_pattern: if not LA_CFG_TEST_PATTERN_C generate
            test_sa_data_l <= sa_data_l;
            test_trg_data_l <= trg_data_l;
        end generate;

-- Trigger
        trg: TRIGGER
        generic map (LA_WBI_ADDR_trigger_base_c)
        port map (
            sa_clk_i => sa_clkdiv_l,
            trg_data_i => test_trg_data_l,
            trigger_o => trg_trigger_l,
            
            wb_clk_i => wb_clk_i,
            wb_rst_i => trg_reset_l,
            wb_i     => wb_i,
            wb_o     => trg_wb_l
        );
        trg_reset_l <= wbi_status_l(7) or wb_rst_i;

-- MUX
-- As the input of the logic analyzer may consist of many banks, this
-- stage selects one that is feed into the sampling pipeline. In the case
-- of the input shift register, several banks can be enabled. They are
-- serially shifted one after another. In this case the trigger is hold
-- back until a shifting cycle is completed.
-- Furthermore the input stage detected changes in the selected input
-- and asserted the changed-flag. This flag can be used by the encoding
-- stage. If not, the optimizer should drop the logic.
        stg_mux_in_l <= (reset_l, test_sa_data_l, trg_trigger_l);

        gen_isr: if LA_CFG_MUX_METHOD_C = ISR generate
            isr : INPUT_SHIFT_REGISTER
            port map (
                sa_clk_i => sa_clkdiv_l,
                stage_i => stg_mux_in_l,
                stage_o => stg_mux_out_l,
                bank_ena_i => isr_bank_ena_l
            );
        end generate;
        
        gen_mux: if LA_CFG_MUX_METHOD_C = MUX generate
            mux : INPUT_MULTIPLEXER
            port map (
                sa_clk_i => sa_clkdiv_l,
                stage_i => stg_mux_in_l,
                stage_o => stg_mux_out_l,
                bank_select_i => mux_bank_select_l
            );
        end generate;

-- ENCODINGS
-- The encoding stage is used to compress the data stream.
-- Therefore a meta-flag is introduced that is stored with every
-- data word. It can be used to tell apart data from meta information,
-- such as a counter for rle. 
-- The additional store-flag informs the memory manager whether to
-- store (='1') or ignore (='0') the data provided.
        gen_rle: if LA_CFG_RLE_C generate
            rle: RUN_LENGTH_ENCODER
            port map (
                sa_clk_i => sa_clkdiv_l,
                stage_i => stg_mux_out_l,
                stage_o => rle_out_l     
            );
        end generate;
        
        enc_out_l <=
            rle_out_l   when LA_CFG_RLE_C   and rle_enable_l = '1'   else
            (stg_mux_out_l.reset, stg_mux_out_l.data, '0', stg_mux_out_l.trigger, '1');
            
        trg_o <= stg_mux_out_l.trigger;

-- MEMORY STORAGE
        mem_mgr : MEMORY_MANAGER
        port map (
            sa_clk_i => sa_clkdiv_l,
            stage_i => enc_out_l,
            base_offset_i => mem_offset_l,
            mem_o => mem_l,
            state_o => mem_state_l
        );
        mem_o <= mem_l;
        
        wbi_status_l(1 downto 0) <= mem_mgr_state_repr_c(mem_state_l.state);
        mem_state_o <= mem_state_l.state;
        mem_clk_o <= sa_clkdiv_l;
end Behavioral;