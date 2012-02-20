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
--   This package contains all internal component descriptions
--   of the logic analyzer. It should be used only by the logic analyzer
--   core and its sub-components.
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
    use IEEE.MATH_REAL.all;    
    
library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.WISHBONE.ALL;
    
package INTERNAL_PKG is
-- HELPER
    function addr_width_f(constant count_c : INTEGER) return INTEGER;
    function iif_f(cond : BOOLEAN; true_value, false_value : INTEGER) return INTEGER;

    type BOOL_TO_LOGIC_T is array(BOOLEAN) of STD_LOGIC;
    constant active_high_c : BOOL_TO_LOGIC_T := (false => '0', true => '1');
    
    constant TRIGGERS_NUM : INTEGER := LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C;

-- WISHBONE ADDRESS CONSTANTS
    constant LA_WBI_SIZE_ISR_BANK_ENA_C : INTEGER := wb_register_width_f(LA_CFG_BANK_COUNT_C);
    constant LA_WBI_SIZE_MUX_BANK_SELECT_C : INTEGER := wb_register_width_f(addr_width_f(LA_CFG_BANK_COUNT_C));

    constant LA_WBI_SIZE_CLK_THRESHOLD_C : INTEGER := wb_register_width_f(LA_CFG_CLKDIV_WIDTH_C);
    constant LA_WBI_SIZE_MEM_ADDR_C : INTEGER := wb_register_width_f(LA_CFG_MEM_ADDR_WIDTH_C);
    constant LA_WBI_SIZE_CURRENT_DATA_C : INTEGER := wb_register_width_f(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C);
    constant LA_WBI_SIZE_TRIGGER_ENA_C : INTEGER := wb_register_width_f(TRIGGERS_NUM);
    constant LA_WBI_SIZE_TRIGGER_SETUP_C : INTEGER := wb_register_width_f(LA_CFG_TRG_WIDTH_C);
    constant LA_WBI_SIZE_TRIGGER_C : INTEGER := wb_register_width_f(TRIGGERS_NUM) + 2*TRIGGERS_NUM*LA_WBI_SIZE_TRIGGER_SETUP_C;

    constant LA_WBI_ADDR_STATUS_C : POSITIVE := 16#10#;
    constant LA_WBI_ADDR_MUX_C : POSITIVE :=                 LA_WBI_ADDR_STATUS_C + 1;
    constant LA_WBI_ADDR_CLK_THRESHOLD_C : POSITIVE :=       LA_WBI_ADDR_MUX_C                  + iif_f(LA_CFG_MUX_METHOD_C = MUX,
                                                                                                    LA_WBI_SIZE_MUX_BANK_SELECT_C,
                                                                                                    LA_WBI_SIZE_ISR_BANK_ENA_C);
    constant LA_WBI_ADDR_MEM_MAX_ADDRESS_C : POSITIVE :=     LA_WBI_ADDR_CLK_THRESHOLD_C        + LA_WBI_SIZE_CLK_THRESHOLD_C;
    constant LA_WBI_ADDR_MEM_BASE_OFFSET_C : POSITIVE :=     LA_WBI_ADDR_MEM_MAX_ADDRESS_C      + LA_WBI_SIZE_MEM_ADDR_C;
    constant LA_WBI_ADDR_MEM_BASE_ADDRESS_C : POSITIVE :=    LA_WBI_ADDR_MEM_BASE_OFFSET_C      + LA_WBI_SIZE_MEM_ADDR_C;
    constant LA_WBI_ADDR_MEM_TRIGGER_ADDRESS_C : POSITIVE := LA_WBI_ADDR_MEM_BASE_ADDRESS_C     + LA_WBI_SIZE_MEM_ADDR_C;
    constant LA_WBI_ADDR_MEM_CURRENT_ADDRESS_C : POSITIVE := LA_WBI_ADDR_MEM_TRIGGER_ADDRESS_C  + LA_WBI_SIZE_MEM_ADDR_C;
    
    constant LA_WBI_ADDR_CURRENT_DATA_C : POSITIVE :=        LA_WBI_ADDR_MEM_CURRENT_ADDRESS_C  + LA_WBI_SIZE_MEM_ADDR_C;
    constant LA_WBI_ADDR_TRIGGER_BASE_C : POSITIVE :=        LA_WBI_ADDR_CURRENT_DATA_C         + LA_WBI_SIZE_CURRENT_DATA_C;
    constant LA_WBI_ADDR_TRIGGER_EDGE_BASE_C : POSITIVE :=   LA_WBI_ADDR_TRIGGER_BASE_C         + LA_WBI_SIZE_TRIGGER_ENA_C;
    constant LA_WBI_ADDR_TRIGGER_VALUE_BASE_C : POSITIVE :=  LA_WBI_ADDR_TRIGGER_EDGE_BASE_C    + 2*LA_CFG_EDGE_TRIGGERS_C*LA_WBI_SIZE_TRIGGER_SETUP_C;
    constant LA_WBI_ADDR_MAX_C : POSITIVE :=                 LA_WBI_ADDR_TRIGGER_VALUE_BASE_C   + 2*LA_CFG_VALUE_TRIGGERS_C*LA_WBI_SIZE_TRIGGER_SETUP_C;


-- FIRST PIPELINE STAGE
    component CLOCK_DIVIDER is
        port (
            sa_clk_i : in  STD_LOGIC;
            sa_clk_o : out  STD_LOGIC;
            
            threshold_i  : in STD_LOGIC_VECTOR(max_f(0, LA_CFG_CLKDIV_WIDTH_C-1) downto 0)
        );
    end component;

    component TEST_PATTERN_GEN is
        port (
            sa_clk_i : in  STD_LOGIC;
              
            sa_data_i : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
            sa_data_o : out STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
            
            trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            trg_data_o : out STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
                
            pattern_select_i : in STD_LOGIC_VECTOR(1 downto 0)
        );
    end component;

-- TRIGGER
    component TRIGGER is
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
    end component;

    component TRIGGER_EDGE is
        port ( 
            sa_clk_i  : in  STD_LOGIC;
            trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
            edge_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            mask_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
            trigger_o : out STD_LOGIC
        );
    end component;
    
    component TRIGGER_VALUE is
        port ( 
            sa_clk_i  : in STD_LOGIC;
            trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
            value_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            mask_i  : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
            trigger_o : out STD_LOGIC
        );
    end component;

-- SECOND STAGE (MUX)
    type MUX_STAGE_IN_T is record
        reset   : STD_LOGIC;
        data    : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        trigger : STD_LOGIC;
    end record;
    
    type MUX_STAGE_OUT_T is record
        reset   : STD_LOGIC;
        data    : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C-1 downto 0);
        new_cycle : STD_LOGIC;
        changed : STD_LOGIC;
        trigger : STD_LOGIC;
    end record;
    
    component INPUT_SHIFT_REGISTER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_IN_T;
        stage_o     : out MUX_STAGE_OUT_T;
        
    -- Config
        bank_ena_i  : in STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C - 1 downto 0)
    );
    end component;
    
    component INPUT_MULTIPLEXER is
        port (
            sa_clk_i    : in  STD_LOGIC;
            stage_i     : in MUX_STAGE_IN_T;
            stage_o     : out MUX_STAGE_OUT_T;
            
        -- Config
            bank_select_i  : in STD_LOGIC_VECTOR(addr_width_f(LA_CFG_BANK_COUNT_C) - 1 downto 0)
        );
    end component;
    
-- THIRD STAGE (ENCODINGS)
    type ENC_STAGE_OUT_T is record
        reset   : STD_LOGIC; 
        data    : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C-1 downto 0);
        meta    : STD_LOGIC;
        trigger : STD_LOGIC;
        store   : STD_LOGIC;
    end record;
    
    component DELTA_ENCODER is
        port (
            sa_clk_i    : in  STD_LOGIC;
            stage_i     : in MUX_STAGE_OUT_T;
            stage_o     : out ENC_STAGE_OUT_T
        );
    end component;
    
    component RUN_LENGTH_ENCODER is
        port (
            sa_clk_i    : in  STD_LOGIC;
            stage_i     : in MUX_STAGE_OUT_T;
            stage_o     : out ENC_STAGE_OUT_T
        );
    end component;
    
    component dual_input_sr is
        generic (
            width_g : NATURAL
        );

        port (
            clk_i : in  STD_LOGIC;
            reset_i : in STD_LOGIC;
            
            input1_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
            ena1_i : STD_LOGIC;
            
            input2_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
            ena2_i : in STD_LOGIC;
            
            output_o : out STD_LOGIC_VECTOR(width_g - 1 downto 0);
            valid_o : out STD_LOGIC
        );
    end component;
    
-- MEMORY MANAGEMENT
    type MEM_STATE_OUT_T is record
        reset : STD_LOGIC;
        state : MEM_STATE_T;
        base_address : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
        trigger_address : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
    end record;

    component MEMORY_MANAGER is
        port (
            sa_clk_i      : in STD_LOGIC;
            stage_i       : in ENC_STAGE_OUT_T;
            base_offset_i : in STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
            
            mem_o       : out MEM_MOSI_T;
            state_o     : out MEM_STATE_OUT_T
        );
    end component;

end INTERNAL_PKG;

package body INTERNAL_PKG is
    function addr_width_f(constant count_c : INTEGER) return INTEGER is
    begin
        return max_f(1, INTEGER(ceil(log2(real(count_c + 1)))));
    end addr_width_f;

    function iif_f(cond : BOOLEAN; true_value, false_value : INTEGER) return INTEGER is
    begin
        if cond then return true_value; else return false_value; end if;
    end function;
end INTERNAL_PKG;