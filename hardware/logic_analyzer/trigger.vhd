--------------------------------------------------------------------------------
-- Company: 
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
-- Engineer: 
--   Manuel Penschuck - manuel at penschuck dot eu
-- Create Date:
--   17.08.2011
--
-- Description:
--   This module instantiates a parametrizable number of
--   edge and value triggers (see generics [LA_CFG_EDGE_TRIGGERS_C]
--   and [LA_CFG_VALUE_TRIGGERS_C]) and holds their configuration in
--   a register bank that can be accessed via a wishbone interface.
--
--   Futhermore there is a "Trigger Enable Mask"-register. It can be
--   used as a bitmap to enable the triggers individually. The least
--   significant bits correspond to edge triggers. If a bit is zero,
--   the trigger in question is bypassed and all of its trigger event
--   are ignored. After reset all triggers are globally disabled and
--   all trigger configuration registers are set to zero.
--   
--   The trigger output [trigger_o] is asserted when at least one
--   enabled trigger fires.
-- 
--   If enabled, the values of all registers can be read via the
--   the wishbone interface. However, this requires a hugh amount
--   of hardware, and can therefore be disabled if not needed.
--
-- Wishbone-Datasheet:
--   Supported Cycles: 
--      Slave, SINGLE WRITE (if [wb_support_read_back] = FALSE)
--      Slave, SINGLE READ/WRITE, RMW (if [wb_support_read_back] = TRUE)
--   Data port, size:   [wb_trg_data_i'LENGTH]-bit
--   Data port, granularity: [wb_trg_data_i'LENGTH]-bit
--   Data port, maximaum operend size: [wb_trg_data_i'LENGTH]-bit
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
--   Address schema:
--     All registers are mapped to the wishbone interface. If a register is
--     wider than the interface, it is sliced and assigned to multiple address
--     using the big-endian scheme, i.e. the least-significant slice is stored
--     at the lower address.
--
--      FROM    | TO        | Mapping
--      --------|-----------|------------------------------------------------------
--      0+ 0x00 | 0+ EW-1   | "Trigger Enable Mask"
--              |           |
--      A+ 0x00 | A+ RW-1   | 1st Edge Trigger: Edge Register
--      A+ RW   | A+ 2*RW-1 | 1st Edge Trigger: Mask Register
--      A+ 2*RW | A+ 3*RW-1 | 2nd Edge Trigger: Required Falling Edge
--        ...   |    ...    |  ... continued in the same fashion
--              |           |
--      B+ 0x00 | B+ RW-1   | 1st Value Trigger: Value Register
--      B+ RW   | B+ 2*RW-1 | 1st Value Trigger: Mask Register
--        ...   |    ...    |  ... continued in the same fashion
--              |           |
--      C       | MAX_ADDR  |  No used (Writing has no effect, reading will give 0)
--
--     Constants: EW = Addresses needed by the "Triggers Enable Mask"
--                     ceil((LA_CFG_EDGE_TRIGGERS_C+LA_CFG_VALUE_TRIGGERS_C + 1) / wb_data_width)
--                RW = Addresses needed by a normal register
--                     ceil(trg_data_i'LENGTH / wb_data_width)
--
--                A = EW
--                B = A + LA_CFG_EDGE_TRIGGERS_C*RW*2
--                C = B + LA_CFG_VALUE_TRIGGERS_C*RW*2
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

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.INTERNAL_PKG.ALL;
    use LOGIC_ANALYZER.WISHBONE.ALL;
    
entity TRIGGER is
    generic (
        wb_base_addr_g : NATURAL := 0
    );

    port (
    -- sampling data
        sa_clk_i : in  STD_LOGIC;
        trg_data_i : in  STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trigger_o : out STD_LOGIC;
        
    -- wishbone interface
        wb_clk_i  : in  STD_LOGIC;
        wb_rst_i  : in  STD_LOGIC;
        wb_o      : out WB_MISO_T;
        wb_i      : in  WB_MOSI_T
    );
    
    -- TODO: Why are passive statements not supported by XST ?
end TRIGGER;

architecture RTL of TRIGGER is
    constant enable_registers_c : POSITIVE := 
        register_width_f(LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C, LA_CFG_WB_DATA_WIDTH_C);
    
    constant trigger_registers_c : POSITIVE :=
        register_width_f(trg_data_i'LENGTH, LA_CFG_WB_DATA_WIDTH_C);
        
    -- Triggers and Enable
    signal triggers_output_l : STD_LOGIC_VECTOR(LA_CFG_EDGE_TRIGGERS_C+LA_CFG_VALUE_TRIGGERS_C-1 downto 0);
    signal triggers_mask_l : STD_LOGIC_VECTOR(LA_CFG_EDGE_TRIGGERS_C+LA_CFG_VALUE_TRIGGERS_C-1 downto 0);

    -- Trigger Settings
    type REGISTERS_T is array(2*(LA_CFG_EDGE_TRIGGERS_C+LA_CFG_VALUE_TRIGGERS_C)-1 downto 0)
                        of STD_LOGIC_VECTOR(trg_data_i'LENGTH - 1 downto 0);
    signal registers_l : REGISTERS_T;
begin
    assert LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C > 0
        report "At least one trigger must be selected"
        severity failure;

  -- WISHBONE INTERFACE
    wb: process(wb_clk_i) is
        variable width_v : integer;
    begin
        if rising_edge(wb_clk_i) then
            if wb_rst_i = '1' then
                triggers_mask_l <= (others => '0');
                
                for i in registers_l'RANGE loop
                    registers_l(i) <= (others => '0');
                end loop;
            else
                if wb_i.stb='1' and wb_i.we='1' then
                -- WRITE REGISTERS FROM WISHBONE
                    -- ENABLE MASK REGISTER
                    wb_assign_write_register_p(wb_base_addr_g, triggers_mask_l, wb_i);
                    
                    -- TRIGGER SETTINGS
                    for i in 0 to 2*(LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C)-1 loop
                        for j in 0 to (trigger_registers_c - 1) loop
                            if wb_i.address = enable_registers_c + i*trigger_registers_c + j + wb_base_addr_g then
                                width_v := min_f((j+1)*wb_i.data'LENGTH, trg_data_i'LENGTH) - j*wb_i.data'LENGTH;
                                registers_l(i)(width_v + j*wb_i.data'LENGTH - 1 downto j*wb_i.data'LENGTH) <= wb_i.data(width_v - 1 downto 0);
                            end if;
                        end loop;
                    end loop;
                end if;
               
                -- This entity supports reading back the settings. However,
                -- this requires a hugh multiplexer in order to access all registers.
                -- As this feature may not be required for normal operation,
                -- it can be disabled by setting [LA_CFG_TRIGGER_WB_READ_C] = FALSE
                if LA_CFG_NO_ESSENTIAL_WB_READ_C then
                    wb_o.data <= (others => '-');

                    -- ENABLE MASK REGISTER
                    wb_assign_read_register_p(wb_base_addr_g, triggers_mask_l, wb_i, wb_o.data);
                    
                    -- TRIGGER SETTINGS
                    for i in 0 to 2*(LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C)-1 loop
                        for j in 0 to (trigger_registers_c - 1) loop
                            if wb_i.address = enable_registers_c + i*trigger_registers_c + j + wb_base_addr_g then
                                width_v := min_f((j+1)*wb_o.data'LENGTH, trg_data_i'LENGTH) - j*wb_o.data'LENGTH;
                                wb_o.data(width_v - 1 downto 0) <= registers_l(i)(width_v + j*wb_o.data'LENGTH - 1 downto j*wb_o.data'LENGTH);
                                if wb_o.data'HIGH >= width_v then
                                    wb_o.data(wb_o.data'HIGH downto width_v) <= (others => '0');
                                end if;
                            end if;
                        end loop;
                    end loop;
                else
                    wb_o.data <= (others => '0');
                end if;
            end if;
        end if;
    end process;
    wb_o.ack <= wb_i.stb;
    
-- Instantiate Triggers
    edge_triggers: for i in 0 to LA_CFG_EDGE_TRIGGERS_C - 1 generate
        my_trigger_edge : TRIGGER_EDGE
        port map (
            sa_clk_i => sa_clk_i,
            trg_data_i => trg_data_i,
            
            edge_i => registers_l(2*i),
            mask_i => registers_l(2*i + 1),
            
            trigger_o => triggers_output_l(i)
        );
    end generate;
    
    value_triggers: for i in LA_CFG_EDGE_TRIGGERS_C to LA_CFG_EDGE_TRIGGERS_C+LA_CFG_VALUE_TRIGGERS_C-1 generate
        my_trigger_value : TRIGGER_VALUE
        port map (
            sa_clk_i => sa_clk_i,
            trg_data_i => trg_data_i,
            
            value_i => registers_l(2*i),
            mask_i => registers_l(2*i+1),
            
            trigger_o => triggers_output_l(i)
        );
    end generate;
    
 -- Combine individual triggers
    process(sa_clk_i) is
        variable result_p : STD_LOGIC;
    begin
        if rising_edge(sa_clk_i) then
            result_p := '0';
            for i in triggers_output_l'RANGE loop
                result_p := result_p OR (triggers_output_l(i) and triggers_mask_l(i));
            end loop;
            result_p := result_p;
            trigger_o <= result_p;
        end if;
    end process;
end RTL;