---------------------------------------------------
-- Embedded Logic Analyzer
-- Includes codes from:
--  -> http://opencores.org/project,miniuart2 downloaded 29.07.2011
--  -> http://www.stefanvhdl.com/vhdl/vhdl/txt_util.vhd downloaded 15.09.2011
---------------------------------------------------

library IEEE;
    use IEEE.STD_LOGIC_1164.all;
    use IEEE.MATH_REAL.all;

library EMBEDDED_LA;

package config_pkg is

    type INPUT_MUX_T is (ISR, MUX);
    CONSTANT LA_CFG_BANK_WIDTH_C : INTEGER range 1 to 255 := 8;
    CONSTANT LA_CFG_BANK_COUNT_C : INTEGER range 1 to 255 := 1;
    CONSTANT LA_CFG_MUX_METHOD_C : INPUT_MUX_T := MUX;

    CONSTANT LA_CFG_WB_ADDR_WIDTH_C : POSITIVE := 15;
    
    -- wishbone standard defines data width of 8, 16, 32, 64
    -- the logic analyzer core support any value bigger
    -- than [LA_CFG_MEM_DATA_WIDTH_C]
    -- however only the standard values have been tested
    CONSTANT LA_CFG_WB_DATA_WIDTH_C : POSITIVE := 8; 

    -- allow reading registers that are not necessarily needed.
    -- disabling reduces hardware footprint, but requires a cold
    -- reset when connecting to the hardware to ensure a defined
    -- state
    CONSTANT LA_CFG_NO_ESSENTIAL_WB_READ_C : BOOLEAN := TRUE;
    

    CONSTANT LA_CFG_CLK_FRQ : POSITIVE := 50_000_000;
    CONSTANT LA_CFG_RS232_BAUD : POSITIVE := 115_200;
    CONSTANT LA_CFG_RS232_DIV : POSITIVE :=
      INTEGER(round(real(LA_CFG_CLK_FRQ) / real(LA_CFG_RS232_BAUD) / 4.0));

    CONSTANT LA_CFG_MEM_DATA_WIDTH_C : INTEGER := 8;
    CONSTANT LA_CFG_MEM_MAX_ADDR_C : INTEGER := 255; --16383;
    CONSTANT LA_CFG_MEM_ADDR_WIDTH_C : INTEGER := 14;

    CONSTANT LA_CFG_RLE_C : BOOLEAN := FALSE; -- include run-length encoding support

    CONSTANT LA_CFG_TRG_WIDTH_C : INTEGER RANGE 1 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 8;
    CONSTANT LA_CFG_EDGE_TRIGGERS_C : INTEGER RANGE 0 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 2;
    CONSTANT LA_CFG_VALUE_TRIGGERS_C : INTEGER RANGE 0 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 2;

    -- if enabled a register synchronously to [sa_clk_i]
    -- to chace the data- and trigger-inputs is placed in front of the
    -- sampling pipeline to ensure, the values captured are with the
    -- rising edge of the external clock
    CONSTANT LA_CFG_INPUT_SYNC_WITH_EXT_CLOCK : BOOLEAN := TRUE;

    -- enable reading of current data via wishbone (not external clock required)
    CONSTANT LA_CFG_READ_CURRENT_DATA : BOOLEAN := TRUE;

    -- if enabled input can be overwritten by test patterns  
    CONSTANT LA_CFG_TEST_PATTERN_C : BOOLEAN := TRUE;    

    CONSTANT LA_CFG_CLKDIV_WIDTH_C : INTEGER RANGE 0 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 0;

end config_pkg;
package body config_pkg is end config_pkg;

  --------------------------------------------------------------------
  -- STOP
  -- STOP
  -- STOP
  -- END OF CONFIGURATION; DON'T CHANGE ANYTHING BEYOND THIS LINE
  -- STOP
  -- STOP
  -- STOP
  --------------------------------------------------------------------
 

library IEEE;
    use IEEE.STD_LOGIC_1164.all;
    
library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;

package LOGIC_ANALYZER_PKG is

    function MIN_F(a,b : INTEGER) return INTEGER;
    function MAX_F(a,b : INTEGER) return INTEGER;

    type MEM_STATE_T is (EMPTY, CAPTURING, FULL);    
    
    type MEM_MOSI_T is record
        address : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
        data    : STD_LOGIC_VECTOR(LA_CFG_MEM_DATA_WIDTH_C - 1 downto 0);
        we      : STD_LOGIC;
    end record;
    
    type WB_MISO_T is record
        data : STD_LOGIC_VECTOR(LA_CFG_WB_DATA_WIDTH_C - 1 downto 0);
        ack  : STD_LOGIC;
    end record;
    
    type WB_MOSI_T is record
        address : STD_LOGIC_VECTOR(LA_CFG_WB_ADDR_WIDTH_C - 1 downto 0);
        data    : STD_LOGIC_VECTOR(LA_CFG_WB_DATA_WIDTH_C - 1 downto 0);
        stb     : STD_LOGIC;
        we      : STD_LOGIC;
    end record;
    
    type WB_MEM_MISO_T is record
        data : STD_LOGIC_VECTOR(LA_CFG_MEM_DATA_WIDTH_C - 1 downto 0);
        ack  : STD_LOGIC;
    end record;
    
    type WB_MEM_MOSI_T is record
        address : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0);
        stb     : STD_LOGIC;
    end record;
    
    component LOGIC_ANALYZER_CORE is
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
            mem_wb_o  : out WB_MEM_MOSI_T;
            
            -- wishbone interface
            wb_clk_i  : in  STD_LOGIC;
            wb_rst_i  : in  STD_LOGIC := '0';
            wb_o      : out WB_MISO_T;
            wb_i      : in  WB_MOSI_T
        );
    end component;    
end LOGIC_ANALYZER_PKG;

package body LOGIC_ANALYZER_PKG is
   function MIN_F(a,b : INTEGER) return INTEGER is
    begin
        if a < b then return a; else return b; end if;
    end function;

    function MAX_F(a,b : INTEGER) return INTEGER is
    begin
        if a > b then return a; else return b; end if;
    end function;
end LOGIC_ANALYZER_PKG;

library IEEE;
    use IEEE.STD_LOGIC_1164.all;

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;

package EMBEDDED_LA_PKG is
    component EMBEDDED_LA_CORE is
        port(
            clk_i       : in  STD_LOGIC;
            
            sa_data_i   : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C * LA_CFG_BANK_COUNT_C - 1 downto 0);
            trg_data_i  : in  STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C - 1 downto 0);

        -- RS232
            rxd_i       : in  STD_LOGIC;
            txd_o       : out STD_LOGIC
        );
    end component;

    type BOOL_TO_LOGIC_T is array(BOOLEAN) of STD_LOGIC;
    constant active_high_c : BOOL_TO_LOGIC_T := (false => '0', true => '1');

    component sampler_memory is
        generic (
            bram_count_g : POSITIVE := 4
        );
        
        port (
            sa_clk_i : in STD_LOGIC;
            mem_i    : in MEM_MOSI_T;
            
            wb_clk_i : in STD_LOGIC;
            wb_rst_i : in STD_LOGIC;
            wb_i     : in WB_MEM_MOSI_T;
            wb_o     : out WB_MEM_MISO_T
        );
    end component;
end EMBEDDED_LA_PKG;

package body EMBEDDED_LA_PKG is
end EMBEDDED_LA_PKG;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

package WISHBONE is

    procedure wb_assign_read_register_p(constant base_addr : NATURAL;
                                        signal reg : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR);

    procedure wb_assign_write_register_p(constant base_addr : NATURAL;
                                         signal reg : out STD_LOGIC_VECTOR;
                                         signal mosi : in WB_MOSI_T);
                                      
    procedure wb_assign_rw_register_p(constant base_addr : NATURAL;
                                      signal reg : inout STD_LOGIC_VECTOR;
                                      signal mosi : in WB_MOSI_T;
                                      signal miso_data : out STD_LOGIC_VECTOR);

    procedure wb_assign_read_variable_p(constant base_addr : NATURAL;
                                        variable reg : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR);

    procedure wb_assign_write_variable_p(constant base_addr : NATURAL;
                                         variable reg : out STD_LOGIC_VECTOR;
                                         signal mosi : in WB_MOSI_T);
                                      
    procedure wb_assign_rw_variable_p(constant base_addr : NATURAL;
                                      variable reg : inout STD_LOGIC_VECTOR;
                                      signal mosi : in WB_MOSI_T;
                                      signal miso_data : out STD_LOGIC_VECTOR);
                                      

    procedure wb_assign_read_constant_p(constant base_addr : NATURAL;
                                        constant value : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR);                                      

    procedure wb_assign_read_constant_p(constant base_addr : NATURAL;
                                        constant value : in INTEGER;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR);                                      

    function register_width_f(
        constant data_width_c : NATURAL;
        constant wb_data_width_c : POSITIVE
    ) return NATURAL;
    
    function wb_register_width_f(
        constant data_width_c : NATURAL
    ) return NATURAL;
end wishbone;

package body wishbone is

    function MIN_F(a,b: INTEGER) return INTEGER is
    begin
        if (a < b) then return a; else return b; end if;
    end MIN_F;
    
    function MAX_F(a,b: INTEGER) return INTEGER is
    begin
        if (a < b) then return b; else return a; end if;
    end MAX_F;
    
    function addr_width_f(constant count_c : INTEGER) return INTEGER is
    begin
        return max_f(1, INTEGER(ceil(log2(real(count_c + 1)))));
    end addr_width_f;
    
    function register_width_f(
        constant data_width_c : NATURAL;
        constant wb_data_width_c : POSITIVE
    ) return NATURAL is
    begin
        return NATURAL(CEIL(REAL(data_width_c) / REAL(WB_DATA_WIDTH_C)));
    end register_width_f;

    function wb_register_width_f(
        constant data_width_c : NATURAL
    ) return NATURAL is
    begin
        return register_width_f(data_width_c, LA_CFG_WB_DATA_WIDTH_C);
    end function;

    procedure wb_assign_read_register_p(constant base_addr : NATURAL;
                                        signal reg : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR
    ) is
        constant reg_width_c : NATURAL := register_width_f(reg'LENGTH, miso_data'LENGTH);
        variable width_v : NATURAL;
    begin
        for i in 0 to reg_width_c - 1 loop
            if to_integer(unsigned(mosi.address)) = base_addr + i then
                width_v := MIN_F((i + 1) * miso_data'LENGTH, reg'LENGTH) - i * miso_data'LENGTH;
                miso_data(width_v - 1 downto 0) <= reg(width_v + i*miso_data'LENGTH - 1 downto i*miso_data'LENGTH);
                if miso_data'HIGH >= width_v then
                    miso_data(miso_data'HIGH downto width_v) <= (others => '0');
                end if;
            end if;
        end loop;
    end wb_assign_read_register_p;

    procedure wb_assign_write_register_p(constant base_addr : NATURAL;
                                         signal reg : out STD_LOGIC_VECTOR;
                                         signal mosi : in WB_MOSI_T
    ) is
        constant reg_width_c : NATURAL := register_width_f(reg'LENGTH, mosi.data'LENGTH);
        variable width_v : NATURAL;
    begin
        if mosi.stb='1' and mosi.we='1' then
            for i in 0 to reg_width_c - 1 loop
                if to_integer(unsigned(mosi.address)) = base_addr + i then
                    width_v := MIN_F((i + 1) * mosi.data'LENGTH, reg'LENGTH) - i * mosi.data'LENGTH;
                    reg(width_v + i*mosi.data'LENGTH - 1 downto i*mosi.data'LENGTH) <= mosi.data(width_v - 1 downto 0);
                end if;
            end loop;
        end if;
    end wb_assign_write_register_p;
    
    procedure wb_assign_rw_register_p(constant base_addr : NATURAL;
                                      signal reg : inout STD_LOGIC_VECTOR;
                                      signal mosi : in WB_MOSI_T;
                                      signal miso_data : out STD_LOGIC_VECTOR
    ) is
    begin
        wb_assign_read_register_p( base_addr, reg, mosi, miso_data);
        wb_assign_write_register_p(base_addr, reg, mosi);
    end wb_assign_rw_register_p;    

    procedure wb_assign_read_variable_p(constant base_addr : NATURAL;
                                        variable reg : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR
    ) is
        constant reg_width_c : NATURAL := register_width_f(reg'LENGTH, miso_data'LENGTH);
        variable width_v : NATURAL;
    begin
        for i in 0 to reg_width_c - 1 loop
            if to_integer(unsigned(mosi.address)) = base_addr + i then
                width_v := MIN_F((i + 1) * miso_data'LENGTH, reg'LENGTH) - i * miso_data'LENGTH;
                miso_data(width_v - 1 downto 0) <= reg(width_v + i*miso_data'LENGTH - 1 downto i*miso_data'LENGTH);
                if miso_data'HIGH >= width_v then
                    miso_data(miso_data'HIGH downto width_v) <= (others => '0');
                end if;
            end if;
        end loop;
    end wb_assign_read_variable_p;

    procedure wb_assign_write_variable_p(constant base_addr : NATURAL;
                                         variable reg : out STD_LOGIC_VECTOR;
                                         signal mosi : in WB_MOSI_T
    ) is
        constant reg_width_c : NATURAL := register_width_f(reg'LENGTH, mosi.data'LENGTH);
        variable width_v : NATURAL;
    begin
        if mosi.stb='1' and mosi.we='1' then
            for i in 0 to reg_width_c - 1 loop
                if to_integer(unsigned(mosi.address)) = base_addr + i then
                    width_v := MIN_F((i + 1) * mosi.data'LENGTH, reg'LENGTH) - i * mosi.data'LENGTH;
                    reg(width_v + i*mosi.data'LENGTH - 1 downto i*mosi.data'LENGTH) := mosi.data(width_v - 1 downto 0);
                end if;
            end loop;
        end if;
    end wb_assign_write_variable_p;
    
    procedure wb_assign_rw_variable_p(constant base_addr : NATURAL;
                                      variable reg : inout STD_LOGIC_VECTOR;
                                      signal mosi : in WB_MOSI_T;
                                      signal miso_data : out STD_LOGIC_VECTOR
    ) is
    begin
        wb_assign_read_variable_p( base_addr, reg, mosi, miso_data);
        wb_assign_write_variable_p(base_addr, reg, mosi);
    end wb_assign_rw_variable_p;   

    procedure wb_assign_read_constant_p(constant base_addr : NATURAL;
                                        constant value : in STD_LOGIC_VECTOR;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR
    ) is
        constant reg_width_c : NATURAL := register_width_f(value'LENGTH, miso_data'LENGTH);
        variable width_v : NATURAL;
    begin
        for i in 0 to reg_width_c - 1 loop
            if to_integer(unsigned(mosi.address)) = base_addr + i then
                width_v := MIN_F((i + 1) * miso_data'LENGTH, value'LENGTH) - i * miso_data'LENGTH;
                miso_data(width_v - 1 downto 0) <= value(width_v + i*miso_data'LENGTH - 1 downto i*miso_data'LENGTH);
                if miso_data'HIGH >= width_v then
                    miso_data(miso_data'HIGH downto width_v) <= (others => '0');
                end if;
            end if;
        end loop;
    end wb_assign_read_constant_p;

    procedure wb_assign_read_constant_p(constant base_addr : NATURAL;
                                        constant value : in INTEGER;
                                        signal mosi : in WB_MOSI_T;
                                        signal miso_data : out STD_LOGIC_VECTOR
    ) is
    begin
        wb_assign_read_constant_p(base_addr, STD_LOGIC_VECTOR(TO_UNSIGNED(value, addr_width_f(value))), mosi, miso_data);
    end procedure;
end WISHBONE;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;    
    use IEEE.MATH_REAL.all;    
    
library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.WISHBONE.ALL;
    
package INTERNAL_PKG is

    function addr_width_f(constant count_c : INTEGER) return INTEGER;
    function iif_f(cond : BOOLEAN; true_value, false_value : INTEGER) return INTEGER;

    type BOOL_TO_LOGIC_T is array(BOOLEAN) of STD_LOGIC;
    constant active_high_c : BOOL_TO_LOGIC_T := (false => '0', true => '1');
    
    constant TRIGGERS_NUM : INTEGER := LA_CFG_EDGE_TRIGGERS_C + LA_CFG_VALUE_TRIGGERS_C;

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

library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

library EMBEDDED_LA;

package txt_util is

    -- prints a message to the screen
    procedure print(text: string);

    -- prints the message when active
    -- useful for debug switches
    procedure print(active: boolean; text: string);

    -- converts std_logic into a character
    function chr(sl: std_logic) return character;

    -- converts std_logic into a string (1 to 1)
    function str(sl: std_logic) return string;

    -- converts std_logic_vector into a string (binary base)
    function str(slv: std_logic_vector) return string;

    -- converts boolean into a string
    function str(b: boolean) return string;

    -- converts an integer into a single character
    -- (can also be used for hex conversion and other bases)
    function chr(int: integer) return character;

    -- converts integer into string using specified base
    function str(int: integer; base: integer) return string;

    -- converts integer to string, using base 10
    function str(int: integer) return string;

    -- convert std_logic_vector into a string in hex format
    function hstr(slv: std_logic_vector) return string;

    -- functions to manipulate strings
    -----------------------------------

    -- convert a character to upper case
    function to_upper(c: character) return character;

    -- convert a character to lower case
    function to_lower(c: character) return character;

    -- convert a string to upper case
    function to_upper(s: string) return string;

    -- convert a string to lower case
    function to_lower(s: string) return string;

   
    
    -- functions to convert strings into other formats
    --------------------------------------------------
    
    -- converts a character into std_logic
    function to_std_logic(c: character) return std_logic; 
    
    -- converts a string into std_logic_vector
    function to_std_logic_vector(s: string) return std_logic_vector; 

  
    -- file I/O
    -----------
       
    -- read variable length string from input file
    procedure str_read(file in_file: TEXT; 
                       res_string: out string);
        
    -- print string to a file and start new line
    procedure print(file out_file: TEXT;
                    new_string: in  string);
    
    -- print character to a file and start new line
    procedure print(file out_file: TEXT;
                    char:       in  character);
                    
end txt_util;

package body txt_util is
   -- prints text to the screen
   procedure print(text: string) is
     variable msg_line: line;
     begin
       write(msg_line, text);
       writeline(output, msg_line);
   end print;

   -- prints text to the screen when active

   procedure print(active: boolean; text: string)  is
     begin
      if active then
         print(text);
      end if;
   end print;

   -- converts std_logic into a character

   function chr(sl: std_logic) return character is
    variable c: character;
    begin
      case sl is
         when 'U' => c:= 'U';
         when 'X' => c:= 'X';
         when '0' => c:= '0';
         when '1' => c:= '1';
         when 'Z' => c:= 'Z';
         when 'W' => c:= 'W';
         when 'L' => c:= 'L';
         when 'H' => c:= 'H';
         when '-' => c:= '-';
      end case;
    return c;
   end chr;

   -- converts std_logic into a string (1 to 1)

   function str(sl: std_logic) return string is
    variable s: string(1 to 1);
    begin
        s(1) := chr(sl);
        return s;
   end str;

   -- converts std_logic_vector into a string (binary base)
   -- (this also takes care of the fact that the range of
   --  a string is natural while a std_logic_vector may
   --  have an integer range)

   function str(slv: std_logic_vector) return string is
     variable result : string (1 to slv'length);
     variable r : integer;
   begin
     r := 1;
     for i in slv'range loop
        result(r) := chr(slv(i));
        r := r + 1;
     end loop;
     return result;
   end str;

   function str(b: boolean) return string is

    begin
       if b then
          return "true";
      else
        return "false";
       end if;
    end str;

   -- converts an integer into a character
   -- for 0 to 9 the obvious mapping is used, higher
   -- values are mapped to the characters A-Z
   -- (this is usefull for systems with base > 10)
   -- (adapted from Steve Vogwell's posting in comp.lang.vhdl)

   function chr(int: integer) return character is
    variable c: character;
   begin
        case int is
          when  0 => c := '0';
          when  1 => c := '1';
          when  2 => c := '2';
          when  3 => c := '3';
          when  4 => c := '4';
          when  5 => c := '5';
          when  6 => c := '6';
          when  7 => c := '7';
          when  8 => c := '8';
          when  9 => c := '9';
          when 10 => c := 'A';
          when 11 => c := 'B';
          when 12 => c := 'C';
          when 13 => c := 'D';
          when 14 => c := 'E';
          when 15 => c := 'F';
          when 16 => c := 'G';
          when 17 => c := 'H';
          when 18 => c := 'I';
          when 19 => c := 'J';
          when 20 => c := 'K';
          when 21 => c := 'L';
          when 22 => c := 'M';
          when 23 => c := 'N';
          when 24 => c := 'O';
          when 25 => c := 'P';
          when 26 => c := 'Q';
          when 27 => c := 'R';
          when 28 => c := 'S';
          when 29 => c := 'T';
          when 30 => c := 'U';
          when 31 => c := 'V';
          when 32 => c := 'W';
          when 33 => c := 'X';
          when 34 => c := 'Y';
          when 35 => c := 'Z';
          when others => c := '?';
        end case;
        return c;
    end chr;

   -- convert integer to string using specified base
   -- (adapted from Steve Vogwell's posting in comp.lang.vhdl)

   function str(int: integer; base: integer) return string is

    variable temp:      string(1 to 10);
    variable num:       integer;
    variable abs_int:   integer;
    variable len:       integer := 1;
    variable power:     integer := 1;

   begin

    -- bug fix for negative numbers
    abs_int := abs(int);

    num     := abs_int;

    while num >= base loop                     -- Determine how many
      len := len + 1;                          -- characters required
      num := num / base;                       -- to represent the
    end loop ;                                 -- number.

    for i in len downto 1 loop                 -- Convert the number to
      temp(i) := chr(abs_int/power mod base);  -- a string starting
      power := power * base;                   -- with the right hand
    end loop ;                                 -- side.

    -- return result and add sign if required
    if int < 0 then
       return '-'& temp(1 to len);
     else
       return temp(1 to len);
    end if;

   end str;

  -- convert integer to string, using base 10
  function str(int: integer) return string is

   begin

    return str(int, 10) ;

   end str;

   -- converts a std_logic_vector into a hex string.
   function hstr(slv: std_logic_vector) return string is
       variable hexlen: integer;
       variable longslv : std_logic_vector(67 downto 0) := (others => '0');
       variable hex : string(1 to 16);
       variable fourbit : std_logic_vector(3 downto 0);
     begin
       hexlen := (slv'left+1)/4;
       if (slv'left+1) mod 4 /= 0 then
         hexlen := hexlen + 1;
       end if;
       longslv(slv'left downto 0) := slv;
       for i in (hexlen -1) downto 0 loop
         fourbit := longslv(((i*4)+3) downto (i*4));
         case fourbit is
           when "0000" => hex(hexlen -I) := '0';
           when "0001" => hex(hexlen -I) := '1';
           when "0010" => hex(hexlen -I) := '2';
           when "0011" => hex(hexlen -I) := '3';
           when "0100" => hex(hexlen -I) := '4';
           when "0101" => hex(hexlen -I) := '5';
           when "0110" => hex(hexlen -I) := '6';
           when "0111" => hex(hexlen -I) := '7';
           when "1000" => hex(hexlen -I) := '8';
           when "1001" => hex(hexlen -I) := '9';
           when "1010" => hex(hexlen -I) := 'A';
           when "1011" => hex(hexlen -I) := 'B';
           when "1100" => hex(hexlen -I) := 'C';
           when "1101" => hex(hexlen -I) := 'D';
           when "1110" => hex(hexlen -I) := 'E';
           when "1111" => hex(hexlen -I) := 'F';
           when "ZZZZ" => hex(hexlen -I) := 'z';
           when "UUUU" => hex(hexlen -I) := 'u';
           when "XXXX" => hex(hexlen -I) := 'x';
           when others => hex(hexlen -I) := '?';
         end case;
       end loop;
       return hex(1 to hexlen);
     end hstr;

   

   -- functions to manipulate strings
   -----------------------------------

   -- convert a character to upper case

   function to_upper(c: character) return character is

      variable u: character;

    begin

       case c is
        when 'a' => u := 'A';
        when 'b' => u := 'B';
        when 'c' => u := 'C';
        when 'd' => u := 'D';
        when 'e' => u := 'E';
        when 'f' => u := 'F';
        when 'g' => u := 'G';
        when 'h' => u := 'H';
        when 'i' => u := 'I';
        when 'j' => u := 'J';
        when 'k' => u := 'K';
        when 'l' => u := 'L';
        when 'm' => u := 'M';
        when 'n' => u := 'N';
        when 'o' => u := 'O';
        when 'p' => u := 'P';
        when 'q' => u := 'Q';
        when 'r' => u := 'R';
        when 's' => u := 'S';
        when 't' => u := 'T';
        when 'u' => u := 'U';
        when 'v' => u := 'V';
        when 'w' => u := 'W';
        when 'x' => u := 'X';
        when 'y' => u := 'Y';
        when 'z' => u := 'Z';
        when others => u := c;
    end case;

      return u;

   end to_upper;

   -- convert a character to lower case

   function to_lower(c: character) return character is

      variable l: character;

    begin

       case c is
        when 'A' => l := 'a';
        when 'B' => l := 'b';
        when 'C' => l := 'c';
        when 'D' => l := 'd';
        when 'E' => l := 'e';
        when 'F' => l := 'f';
        when 'G' => l := 'g';
        when 'H' => l := 'h';
        when 'I' => l := 'i';
        when 'J' => l := 'j';
        when 'K' => l := 'k';
        when 'L' => l := 'l';
        when 'M' => l := 'm';
        when 'N' => l := 'n';
        when 'O' => l := 'o';
        when 'P' => l := 'p';
        when 'Q' => l := 'q';
        when 'R' => l := 'r';
        when 'S' => l := 's';
        when 'T' => l := 't';
        when 'U' => l := 'u';
        when 'V' => l := 'v';
        when 'W' => l := 'w';
        when 'X' => l := 'x';
        when 'Y' => l := 'y';
        when 'Z' => l := 'z';
        when others => l := c;
    end case;

      return l;

   end to_lower;

   -- convert a string to upper case

   function to_upper(s: string) return string is

     variable uppercase: string (s'range);

   begin

     for i in s'range loop
        uppercase(i):= to_upper(s(i));
     end loop;
     return uppercase;

   end to_upper;

   -- convert a string to lower case

   function to_lower(s: string) return string is

     variable lowercase: string (s'range);

   begin

     for i in s'range loop
        lowercase(i):= to_lower(s(i));
     end loop;
     return lowercase;

   end to_lower;

function to_std_logic(c: character) return std_logic is 
    variable sl: std_logic;
    begin
      case c is
        when 'U' => 
           sl := 'U'; 
        when 'X' =>
           sl := 'X';
        when '0' => 
           sl := '0';
        when '1' => 
           sl := '1';
        when 'Z' => 
           sl := 'Z';
        when 'W' => 
           sl := 'W';
        when 'L' => 
           sl := 'L';
        when 'H' => 
           sl := 'H';
        when '-' => 
           sl := '-';
        when others =>
           sl := 'X'; 
    end case;
   return sl;
  end to_std_logic;

function to_std_logic_vector(s: string) return std_logic_vector is 
  variable slv: std_logic_vector(s'high-s'low downto 0);
  variable k: integer;
begin
   k := s'high-s'low;
  for i in s'range loop
     slv(k) := to_std_logic(s(i));
     k      := k - 1;
  end loop;
  return slv;
end to_std_logic_vector;                                       
                                       
                                       
                                       
                                       
                                       
                                       

     
procedure str_read(file in_file: TEXT; 
                   res_string: out string) is
       
       variable l:         line;
       variable c:         character;
       variable is_string: boolean;
       
   begin
           
     readline(in_file, l);
     -- clear the contents of the result string
     for i in res_string'range loop
         res_string(i) := ' ';
     end loop;   
     -- read all characters of the line, up to the length  
     -- of the results string
     for i in res_string'range loop
        read(l, c, is_string);
        res_string(i) := c;
        if not is_string then -- found end of line
           exit;
        end if;   
     end loop; 
                     
end str_read;

procedure print(file out_file: TEXT;
                new_string: in  string) is
       
       variable l: line;
       
   begin
      
     write(l, new_string);
     writeline(out_file, l);
                     
end print;

procedure print(file out_file: TEXT;
                char: in  character) is
       
       variable l: line;
       
   begin
      
     write(l, char);
     writeline(out_file, l);
                     
end print;

procedure str_write(file out_file: TEXT; 
                    new_string: in  string) is
 begin
      
   for i in new_string'range loop
      print(out_file, new_string(i));
      if new_string(i) = LF then -- end of string
         exit;
      end if;
   end loop;               
end str_write;

end txt_util;

library IEEE;
    use IEEE.STD_LOGIC_1164.all;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    
library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

library EMBEDDED_LA;

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

library ieee;
use ieee.std_logic_1164.all;

library EMBEDDED_LA;

package ucrc_pkg is

   component ucrc_ser
      generic (
         POLYNOMIAL : std_logic_vector;       -- 4 to 32 bits
         INIT_VALUE : std_logic_vector;
         SYNC_RESET : integer range 0 to 1);  -- use synchronous reset
      port (
         clk_i   : in  std_logic;       -- clock
         rst_i   : in  std_logic;       -- init CRC
         clken_i : in  std_logic;       -- clock enable
         data_i  : in  std_logic;       -- data input
         flush_i : in  std_logic;       -- flush crc
         match_o : out std_logic;       -- CRC match flag
         crc_o   : out std_logic_vector(POLYNOMIAL'length - 1 downto 0));  -- CRC output
   end component;

   component ucrc_par
      generic (
         POLYNOMIAL : std_logic_vector;
         INIT_VALUE : std_logic_vector;
         DATA_WIDTH : integer range 2 to 256;
         SYNC_RESET : integer range 0 to 1);  -- use synchronous reset
      port (
         clk_i   : in  std_logic;       -- clock
         rst_i   : in  std_logic;       -- init CRC
         clken_i : in  std_logic;       -- clock enable
         data_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);  -- data input
         match_o : out std_logic;       -- CRC match flag
         crc_o   : out std_logic_vector(POLYNOMIAL'length - 1 downto 0));  -- CRC output
   end component;

end ucrc_pkg;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;

entity CLOCK_DIVIDER is
    port (
        sa_clk_i : in  STD_LOGIC;
        sa_clk_o : out  STD_LOGIC;
        
        threshold_i  : in STD_LOGIC_VECTOR(max_f(0, LA_CFG_CLKDIV_WIDTH_C-1) downto 0)
    );
end CLOCK_DIVIDER;

architecture Behavioral of clock_divider is
    signal counter_l, threshold_l : STD_LOGIC_VECTOR(LA_CFG_CLKDIV_WIDTH_C-1 downto 0) := (others => '0');
    signal sa_clk_l : STD_LOGIC;
begin
    sa_clk_o <= sa_clk_i when threshold_i = STD_LOGIC_VECTOR(TO_UNSIGNED(0, counter_l'LENGTH)) else sa_clk_l;

    process(sa_clk_i) is
    begin
        if rising_edge(sa_clk_i) then
            threshold_l <= threshold_i;
            if UNSIGNED(counter_l) = UNSIGNED(threshold_l) then
                counter_l <= (others => '0');
                sa_clk_l <= '1';
            else
                counter_l <= STD_LOGIC_VECTOR(UNSIGNED(counter_l) + TO_UNSIGNED(1, counter_l'LENGTH));
                sa_clk_l <= '0';
            end if;
        end if;
    end process;
end Behavioral;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;

entity INPUT_MULTIPLEXER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_IN_T;
        stage_o     : out MUX_STAGE_OUT_T;
        
    -- Config
        bank_select_i  : in STD_LOGIC_VECTOR(addr_width_f(LA_CFG_BANK_COUNT_C) - 1 downto 0)
    );
end INPUT_MULTIPLEXER;

architecture RTL of INPUT_MULTIPLEXER is
begin
    process (sa_clk_i) is
        variable post_reset_v : STD_LOGIC := '1';
        variable trigger_v : STD_LOGIC := '0';
        variable old_bank_v : STD_LOGIC_VECTOR(bank_select_i'RANGE);
        variable selected_v, old_v : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C - 1 downto 0);
    begin
        stage_o.new_cycle <= '1';
    
        if rising_edge(sa_clk_i) then
        -- delay values by one clock cycle to maintain correlation
            stage_o.reset <= stage_i.reset;
			trigger_v := (not stage_i.reset) AND (trigger_v or stage_i.trigger);
            stage_o.trigger <= trigger_v;

        -- select active bank
            selected_v := (others => '-'); -- prevent latch
            for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                if STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_select_i'LENGTH)) = bank_select_i then
                    selected_v := stage_i.data((i+1)* selected_v'LENGTH - 1 downto i*selected_v'LENGTH);
                end if;
            end loop;
            
            -- synopsys translate_off
            assert to_integer(unsigned(bank_select_i)) < LA_CFG_BANK_COUNT_C
                report "MUX: invalid bank selected" severity error;
            -- synopsys translate_on

            stage_o.data <= selected_v;

        -- detect changes
            stage_o.changed <= active_high_c(post_reset_v = '1' or old_v /= selected_v or old_bank_v /= bank_select_i);
            old_v := selected_v;
            old_bank_v := bank_select_i;
            post_reset_v := stage_i.reset;
        end if;
    end process;
end RTL;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;    

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    
entity RUN_LENGTH_ENCODER is
    port (
        sa_clk_i    : in  STD_LOGIC;
        stage_i     : in MUX_STAGE_OUT_T;
        stage_o     : out ENC_STAGE_OUT_T
    );
end RUN_LENGTH_ENCODER;

architecture behav of RUN_LENGTH_ENCODER is

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

    signal sr_counter_l, sr_data_l, sr_out_l : STD_LOGIC_VECTOR(stage_i.data'HIGH+2 downto 0);
begin
    assert not (LA_CFG_RLE_C and LA_CFG_MUX_METHOD_C = ISR)
        report "Run length encoding does not fully support the input shift register. If just one bank changed all are stored."
        severity warning;

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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    
entity TRIGGER_EDGE is
    port ( 
        sa_clk_i  : in  STD_LOGIC;
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        edge_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        mask_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trigger_o : out STD_LOGIC
    );
end TRIGGER_EDGE;

architecture RTL of TRIGGER_EDGE is
    signal trg_data_old_l : STD_LOGIC_VECTOR(trg_data_i'RANGE);
begin
    process(sa_clk_i) is
        variable
            change_p,
            change_rising_p,
            change_falling_p : STD_LOGIC_VECTOR(trg_data_i'RANGE);
            
        variable result_p : STD_LOGIC;
    begin
        if rising_edge(sa_clk_i) then
            -- store current data layout 
            trg_data_old_l <= trg_data_i;
            
            -- calculate changes
            change_p := trg_data_i xor trg_data_old_l;
            change_rising_p := change_p and trg_data_i;
            change_falling_p := change_p and (not trg_data_i);
            
            result_p := '1';
            for i in change_p'RANGE loop
                result_p := result_p and 
                    ( (change_rising_p(i) and edge_i(i))
                      or (change_falling_p(i) or not edge_i(i))
                      or not mask_i(i) );
            end loop;
            
            trigger_o <= result_p;
        end if;
    end process;
end RTL;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;

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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    
entity TRIGGER_VALUE is
    port ( 
        sa_clk_i  : in STD_LOGIC;
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        value_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        mask_i  : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        
        trigger_o : out STD_LOGIC
    );
end TRIGGER_VALUE;

architecture RTL of TRIGGER_VALUE is
begin
    process(sa_clk_i) is
        variable result_p : STD_LOGIC;
        variable equal_p : STD_LOGIC_VECTOR(trg_data_i'RANGE);
    begin
        if rising_edge(sa_clk_i) then
            result_p := '1';
            
            -- bitwise equal
            equal_p := (trg_data_i and value_i) or not (trg_data_i or value_i);
            
            for i in trg_data_i'RANGE loop
                result_p := result_p and (not mask_i(i) or equal_p(i));
            end loop;
            
            trigger_o <= result_p;
        end if;
    end process;
end RTL;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

entity dual_input_sr is
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
end dual_input_sr;

architecture Behavioral of dual_input_sr is
    type SR_T is array(1 downto 0) of STD_LOGIC_VECTOR(width_g - 1 downto 0);
    signal sr_l : SR_T;
    signal sr_level_l : integer range 0 to 2 := 0; -- stores how many valid values there are in the sr
begin
    process(clk_i) is
    begin
        if rising_edge(clk_i) then
            if reset_i = '1' then
                sr_level_l <= 0;
                sr_l <= (others => (others => '-'));
                valid_o <= '0';
            else
                valid_o <= '1';
                
                if sr_level_l = 1 or sr_level_l = 0 then
                    -- can load input1 and input2
                    if ena1_i = '1' and ena2_i = '1' then
                        sr_l <= (0 => input1_i, 1 => input2_i);
                        sr_level_l <= 2;
                    elsif ena1_i = '1' then
                        sr_l <= (0 => input1_i, 1 => (others => '-'));
                        sr_level_l <= 1;
                    elsif ena2_i = '1' then
                        sr_l <= (0 => input2_i, 1 => (others => '-'));
                        sr_level_l <= 1;
                    else
                        sr_l <= (others => (others => '-'));
                        sr_level_l <= 0;
                        valid_o <= '0';
                    end if;
                elsif sr_level_l = 2 then
                    if ena1_i = '1' then
                        sr_l <= (0 => sr_l(1), 1 => input1_i);
                        sr_level_l <= 2;
                    elsif ena2_i = '1' then
                        sr_l <= (0 => sr_l(1), 1 => input2_i);
                        sr_level_l <= 2;
                    else
                        sr_l <= (0 => sr_l(1), 1 => (others => '-'));
                        sr_level_l <= 1;
                    end if;
                end if;
            end if;
        end if;
    end process;

    output_o <= sr_l(0);
end Behavioral;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;    

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;

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

    signal overflow_l : STD_LOGIC := '0';  -- indicates whether ring puffer is currently
                                           -- overwriting old values
    signal write_address_l : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0) := (others => '0');
    

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

    mem_o.address <= write_address_l;
    mem_o.we <= stage_i.store and active_high_c(state_l /= FULL);
    
    mem_o.data(stage_i.data'RANGE) <= stage_i.data;
    mem_o.data(mem_o.data'HIGH downto stage_i.data'LENGTH) <= (others => stage_i.meta);

    

    state_o.state <= state_l;
    state_o.base_address <= stop_address_l;
end Behavioral;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;
    
library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    use EMBEDDED_LA.TXT_UTIL.ALL;

entity TEST_PATTERN_GEN is
    port (
        sa_clk_i : in  STD_LOGIC;
          
        sa_data_i : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        sa_data_o : out STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C*LA_CFG_BANK_COUNT_C-1 downto 0);
        
        trg_data_i : in STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
        trg_data_o : out STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
            
        pattern_select_i : in STD_LOGIC_VECTOR(1 downto 0)
    );
end TEST_PATTERN_GEN;

architecture rtl of TEST_PATTERN_GEN is
    constant bank_addr_width_c : NATURAL := addr_width_f(LA_CFG_BANK_COUNT_C-1);
    constant counter_width_c : NATURAL := LA_CFG_BANK_WIDTH_C - bank_addr_width_c;
    
    signal counter_l : STD_LOGIC_VECTOR(counter_width_c * LA_CFG_BANK_COUNT_C + 3 downto 0) := (others => '0');
begin
    assert LA_CFG_BANK_WIDTH_C > bank_addr_width_c
    report "Test-Pattern Generator needs LA_CFG_BANK_WIDTH_C > log2(LA_CFG_BANK_COUNT_C)"
    severity failure;

    assert false
    report "Test-Pattern: Bank[" & str(LA_CFG_BANK_WIDTH_C-1) & " downto " & str(LA_CFG_BANK_WIDTH_C-bank_addr_width_c) & "] "
                    & "Counter[" & str(counter_width_c - 1) & " downto 0]" severity note;
                    
                  

    process(sa_clk_i) is
        variable tmp : STD_LOGIC_VECTOR(sa_data_o'RANGE);
    begin
        if rising_edge(sa_clk_i) then
            tmp := (others => '-');
            case(pattern_select_i) is
                when "01" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'RANGE);
                    else
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l(counter_width_c - 1 downto 0);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                
                when "10" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'RANGE);
                    else                
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l( (i+1) * counter_width_c - 1 downto i * counter_width_c);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                    
                when "11" =>
                    if LA_CFG_BANK_COUNT_C = 1 then
                        tmp := counter_l(sa_data_o'HIGH + 2 downto 2);
                    else
                        for i in 0 to LA_CFG_BANK_COUNT_C - 1 loop
                            tmp( (i+1)*LA_CFG_BANK_WIDTH_C - 1 downto i * LA_CFG_BANK_WIDTH_C) := 
                                  STD_LOGIC_VECTOR(TO_UNSIGNED(i, bank_addr_width_c)) 
                                & counter_l(counter_width_c + 1 downto 2);
                        end loop;
                    end if;
                    
                    sa_data_o <= tmp;
                    for i in 0 to integer( floor(real(trg_data_o'LENGTH) / real(tmp'LENGTH)) ) loop
                        trg_data_o(min_f(trg_data_o'HIGH, tmp'LENGTH * (i+1)-1) downto tmp'LENGTH*i) <=
                            tmp(min_f(tmp'LENGTH*(i+1)-1, trg_data_o'HIGH) - tmp'LENGTH*i downto 0);
                    end loop;
                    
                when others =>
                    sa_data_o <= sa_data_i;
                    trg_data_o <= trg_data_i;
            end case;

            counter_l <= STD_LOGIC_VECTOR(UNSIGNED(counter_l) + TO_UNSIGNED(1, counter_l'LENGTH));
        end if;
    end process;
end rtl;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    use EMBEDDED_LA.WISHBONE.ALL;
    
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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;     

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.INTERNAL_PKG.ALL;
    use EMBEDDED_LA.WISHBONE.ALL;
    use EMBEDDED_LA.TXT_UTIL.ALL;
    
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

    signal sa_clkdiv_l, sa_clkdiv_unbuffered_l : STD_LOGIC;
    signal clk_threshold_l : STD_LOGIC_VECTOR(max_f(0, LA_CFG_CLKDIV_WIDTH_C-1) downto 0) := (others => '0');

    alias  test_pattern_select_l : STD_LOGIC_VECTOR(1 downto 0) is wbi_status_l(5 downto 4);
    signal test_sa_data_l  : STD_LOGIC_VECTOR(sa_data_i'RANGE);
    signal test_trg_data_l : STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C-1 downto 0);
    

    signal trg_trigger_l : STD_LOGIC;
    signal trg_reset_l : STD_LOGIC;
    signal trg_wb_l : WB_MISO_T;
    

    signal stg_mux_in_l : MUX_STAGE_IN_T;
    signal stg_mux_out_l : MUX_STAGE_OUT_T;
    
    signal isr_bank_ena_l : STD_LOGIC_VECTOR(LA_CFG_BANK_COUNT_C - 1 downto 0) := (others => '1');
    signal mux_bank_select_l : STD_LOGIC_VECTOR(addr_width_f(LA_CFG_BANK_COUNT_C) - 1 downto 0) := (others => '0');

    alias  rle_enable_l : STD_LOGIC is wbi_status_l(3);
    signal rle_out_l : ENC_STAGE_OUT_T;
    
    signal enc_out_l : ENC_STAGE_OUT_T;

    signal mem_offset_l : STD_LOGIC_VECTOR(LA_CFG_MEM_ADDR_WIDTH_C - 1 downto 0) := STD_LOGIC_VECTOR(TO_UNSIGNED(LA_CFG_MEM_MAX_ADDR_C / 2, LA_CFG_MEM_ADDR_WIDTH_C));
    signal mem_state_l  : MEM_STATE_OUT_T;
    signal mem_l : MEM_MOSI_T;
    

    signal reset_l : STD_LOGIC;
begin

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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    
library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;

entity char_to_hex is
    port (
        char_i : in  STD_LOGIC_VECTOR (7 downto 0);
        
        hex_o   : out STD_LOGIC_VECTOR (3 downto 0);
        valid_o : out STD_LOGIC
    );
end char_to_hex;

architecture Behavioral of char_to_hex is
begin
    process(char_i) is
    begin
           if is_eq_char(char_i, '0') then hex_o <= X"0"; valid_o <= '1';
        elsif is_eq_char(char_i, '1') then hex_o <= X"1"; valid_o <= '1';
        elsif is_eq_char(char_i, '2') then hex_o <= X"2"; valid_o <= '1';
        elsif is_eq_char(char_i, '3') then hex_o <= X"3"; valid_o <= '1';
        elsif is_eq_char(char_i, '4') then hex_o <= X"4"; valid_o <= '1';
        elsif is_eq_char(char_i, '5') then hex_o <= X"5"; valid_o <= '1';
        elsif is_eq_char(char_i, '6') then hex_o <= X"6"; valid_o <= '1';
        elsif is_eq_char(char_i, '7') then hex_o <= X"7"; valid_o <= '1';
        elsif is_eq_char(char_i, '8') then hex_o <= X"8"; valid_o <= '1';
        elsif is_eq_char(char_i, '9') then hex_o <= X"9"; valid_o <= '1';
        elsif is_eq_char(char_i, 'A') then hex_o <= X"A"; valid_o <= '1';
        elsif is_eq_char(char_i, 'B') then hex_o <= X"B"; valid_o <= '1';
        elsif is_eq_char(char_i, 'C') then hex_o <= X"C"; valid_o <= '1';
        elsif is_eq_char(char_i, 'D') then hex_o <= X"D"; valid_o <= '1';
        elsif is_eq_char(char_i, 'E') then hex_o <= X"E"; valid_o <= '1';
        elsif is_eq_char(char_i, 'F') then hex_o <= X"F"; valid_o <= '1';
        else                               hex_o <= X"0"; valid_o <= '0'; end if;
    end process;
end Behavioral;

library ieee;
   use ieee.std_logic_1164.all;

library EMBEDDED_LA;

entity RxUnit is
  port (
     Clk    : in  std_logic;  -- system clock signal
     Reset  : in  std_logic;  -- Reset input
     Enable : in  std_logic;  -- Enable input
     ReadA  : in  Std_logic;  -- Async Read Received Byte
     RxD    : in  std_logic;  -- RS-232 data input
     RxAv   : out std_logic;  -- Byte available
     DataO  : out std_logic_vector(7 downto 0)); -- Byte received
end RxUnit;

architecture Behaviour of RxUnit is
  signal RReg    : std_logic_vector(7 downto 0); -- receive register  
  signal RRegL   : std_logic;                    -- Byte received
begin
  -- RxAv process
  RxAvProc : process(RRegL,Reset,ReadA)
  begin
     if ReadA = '1' or Reset = '1' then
        RxAv <= '0';  -- Negate RxAv when RReg read     
     elsif Rising_Edge(RRegL) then
        RxAv <= '1';  -- Assert RxAv when RReg written
     end if;
  end process;
  
  -- Rx Process
  RxProc : process(Clk,Reset,Enable,RxD,RReg)
  variable BitPos : INTEGER range 0 to 10;   -- Position of the bit in the frame
  variable SampleCnt : INTEGER range 0 to 3; -- Count from 0 to 3 in each bit 
  begin
     if Reset = '1' then -- Reset
        RRegL <= '0';
        BitPos := 0;
     elsif Rising_Edge(Clk) then
        if Enable = '1' then
           case BitPos is
              when 0 => -- idle
                 RRegL <= '0';
                 if RxD = '0' then -- Start Bit
                    SampleCnt := 0;
                    BitPos := 1;
                 end if;
              when 10 => -- Stop Bit
                 BitPos := 0;    -- next is idle
                 RRegL <= '1';   -- Indicate byte received
                 DataO <= RReg;  -- Store received byte
              when others =>
                 if (SampleCnt = 1 and BitPos >= 2) then -- Sample RxD on 1
                    RReg(BitPos-2) <= RxD; -- Deserialisation
                 end if;
                 if SampleCnt = 3 then -- Increment BitPos on 3
                    BitPos := BitPos + 1;
                 end if;
           end case;
           if SampleCnt = 3 then
              SampleCnt := 0;
           else
              sampleCnt := SampleCnt + 1;
           end if;
           
        end if;
     end if;
  end process;
end Behaviour;

library ieee;
use ieee.std_logic_1164.all;

library EMBEDDED_LA;

entity TxUnit is
  port (
     Clk    : in  std_logic;  -- Clock signal
     Reset  : in  std_logic;  -- Reset input
     Enable : in  std_logic;  -- Enable input
     LoadA  : in  std_logic;  -- Asynchronous Load
     TxD    : out std_logic;  -- RS-232 data output
     Busy   : out std_logic;  -- Tx Busy
     DataI  : in  std_logic_vector(7 downto 0)); -- Byte to transmit
end TxUnit;

architecture Behaviour of TxUnit is

  component synchroniser
  port (
     C1 : in std_logic;	 -- Asynchronous signal
     C :  in std_logic;	 -- Clock
     O :  out Std_logic);
  end component;
  
  signal TBuff    : std_logic_vector(7 downto 0); -- transmit buffer
  signal TReg     : std_logic_vector(7 downto 0); -- transmit register
  signal TBufL    : std_logic;  -- Buffer loaded
  signal LoadS    : std_logic;	-- Synchronised load signal

begin
  -- Synchronise Load on Clk
  SyncLoad : Synchroniser port map (LoadA, Clk, LoadS);
  Busy <= LoadS or TBufL;

  -- Tx process
  TxProc : process(Clk, Reset, Enable, DataI, TBuff, TReg, TBufL)
  variable BitPos : INTEGER range 0 to 10; -- Bit position in the frame
  begin
     if Reset = '1' then
        TBufL <= '0';
        BitPos := 0;
        TxD <= '1';
     elsif Rising_Edge(Clk) then
        if LoadS = '1' then
           TBuff <= DataI;
           TBufL <= '1';
        end if;
        if Enable = '1' then
           case BitPos is
              when 0 => -- idle or stop bit
                 TxD <= '1';
                 if TBufL = '1' then -- start transmit. next is start bit
                    TReg <= TBuff;
                    TBufL <= '0';
                    BitPos := 1;
                 end if;
              when 1 => -- Start bit
                 TxD <= '0';
                 BitPos := 2;
              when others =>
                 TxD <= TReg(BitPos-2); -- Serialisation of TReg
                 BitPos := BitPos + 1;
           end case;
           if BitPos = 10 then -- bit8. next is stop bit
              BitPos := 0;
           end if;
        end if;
     end if;
  end process;
end Behaviour;

library IEEE,STD;
use IEEE.std_logic_1164.all;

library EMBEDDED_LA;

entity synchroniser is
   port (
      C1 : in std_logic;
      C :  in std_logic;
      O :  out std_logic);
end synchroniser;

architecture Behaviour of synchroniser is
   signal C1A : std_logic;
   signal C1S : std_logic;
   signal R : std_logic;
begin
   RiseC1A : process(C1,R)
   begin
      if Rising_Edge(C1) then
         C1A <= '1';
      end if;
      if (R = '1') then
         C1A <= '0';
      end if;
   end process;

   SyncP : process(C,R)
   begin
      if Rising_Edge(C) then
         if (C1A = '1') then
            C1S <= '1';
         else C1S <= '0';
         end if;
         if (C1S = '1') then
            R <= '1';
         else R <= '0';
         end if;
      end if;
      if (R = '1') then
         C1S <= '0';
      end if;
   end process;
   O <= C1S;
end Behaviour;

library IEEE,STD;
use IEEE.std_logic_1164.all;

library EMBEDDED_LA;

entity Counter is
  generic(Count: INTEGER range 0 to 65535); -- Count revolution
  port (
     Clk      : in  std_logic;  -- Clock
     Reset    : in  std_logic;  -- Reset input
     CE       : in  std_logic;  -- Chip Enable
     O        : out std_logic); -- Output
end Counter;

architecture Behaviour of Counter is
begin
  counter : process(Clk,Reset)
     variable Cnt : INTEGER range 0 to Count-1;
  begin
     if Reset = '1' then
        Cnt := Count - 1;
        O <= '0';
     elsif Rising_Edge(Clk) then
        if CE = '1' then
           if Cnt = 0 then
              O <= '1';
              Cnt := Count - 1;
           else
              O <= '0';
              Cnt := Cnt - 1;
           end if;
        else O <= '0';
        end if;
     end if;
  end process;
end Behaviour;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;

entity hex_to_char is
    generic (
        uppercase_g : boolean := TRUE
    );
    port (
        hex_i : in  STD_LOGIC_VECTOR (3 downto 0);
        char_o : out  STD_LOGIC_VECTOR (7 downto 0)
    );
end hex_to_char;

architecture Behavioral of hex_to_char is
begin
    char_o <=
        TO_STD_LOGIC_VECTOR('0') when hex_i=X"0" else
        TO_STD_LOGIC_VECTOR('1') when hex_i=X"1" else
        TO_STD_LOGIC_VECTOR('2')  when hex_i=X"2" else
        TO_STD_LOGIC_VECTOR('3')  when hex_i=X"3" else
        TO_STD_LOGIC_VECTOR('4')  when hex_i=X"4" else
        TO_STD_LOGIC_VECTOR('5')  when hex_i=X"5" else
        TO_STD_LOGIC_VECTOR('6')  when hex_i=X"6" else
        TO_STD_LOGIC_VECTOR('7')  when hex_i=X"7" else
        TO_STD_LOGIC_VECTOR('8')  when hex_i=X"8" else
        TO_STD_LOGIC_VECTOR('9')  when hex_i=X"9" else
        TO_STD_LOGIC_VECTOR('A')  when hex_i=X"A" and uppercase_g else
        TO_STD_LOGIC_VECTOR('B')  when hex_i=X"B" and uppercase_g else
        TO_STD_LOGIC_VECTOR('C')  when hex_i=X"C" and uppercase_g else
        TO_STD_LOGIC_VECTOR('D')  when hex_i=X"D" and uppercase_g else
        TO_STD_LOGIC_VECTOR('E')  when hex_i=X"E" and uppercase_g else
        TO_STD_LOGIC_VECTOR('F')  when hex_i=X"F" and uppercase_g else
        TO_STD_LOGIC_VECTOR('a')  when hex_i=X"A" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('b')  when hex_i=X"B" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('c')  when hex_i=X"C" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('d')  when hex_i=X"D" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('e')  when hex_i=X"E" and not uppercase_g else
        TO_STD_LOGIC_VECTOR('f') ;
end Behavioral;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;

entity shift_register is
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
end shift_register;

architecture Behavioral of shift_register is
    signal data_l : STD_LOGIC_VECTOR(4*nibbles_g-1 downto 0);
begin
    process(clk_i) is 
    begin
        if rising_edge(clk_i) then
            case(operation_i) is
                when RESET => data_l <= (others => '0');
                when LOAD  => data_l <= data_i & "0000";
                when SHIFT => data_l <= data_l(data_l'HIGH - 4 downto 0) & "0000";
                when LOADSHIFT => data_l <= data_l(data_l'HIGH - 4 downto 0) & shift_data_i;
                when others => NULL;
            end case;
        end if;
    end process;
    
    data_o <= data_l(data_l'HIGH downto 4);
end Behavioral;

library ieee;
use ieee.std_logic_1164.all;

library EMBEDDED_LA;

entity ucrc_par is
   generic (
      POLYNOMIAL : std_logic_vector;
      INIT_VALUE : std_logic_vector;
      DATA_WIDTH : integer range 2 to 256;
      SYNC_RESET : integer range 0 to 1);  -- use sync./async reset
   port (
      clk_i   : in  std_logic;          -- clock
      rst_i   : in  std_logic;          -- init CRC
      clken_i : in  std_logic;          -- clock enable
      data_i  : in  std_logic_vector(DATA_WIDTH - 1 downto 0);  -- data input
      match_o : out std_logic;          -- CRC match flag
      crc_o   : out std_logic_vector(POLYNOMIAL'length - 1 downto 0));  -- CRC output
end ucrc_par;

architecture rtl of ucrc_par is

   constant msb      : integer                        := POLYNOMIAL'length - 1;
   constant init_msb : integer                        := INIT_VALUE'length - 1;
   constant p        : std_logic_vector(msb downto 0) := POLYNOMIAL;
   constant dw       : integer                        := DATA_WIDTH;
   constant pw       : integer                        := POLYNOMIAL'length;
   type fb_array is array (dw downto 1) of std_logic_vector(msb downto 0);
   type dmsb_array is array (dw downto 1) of std_logic_vector(msb downto 1);
   signal crca       : fb_array;
   signal da, ma     : dmsb_array;
   signal crc, zero  : std_logic_vector(msb downto 0);
   signal arst, srst : std_logic;
   
begin

   PCHK1 : if msb /= init_msb generate
      process
      begin
         report "POLYNOMIAL and INIT_VALUE vectors must be equal length!"
            severity failure;
         wait;
      end process;
   end generate PCHK1;

   PCHK2 : if (msb < 3) or (msb > 31) generate
      process
      begin
         report "POLYNOMIAL must be of order 4 to 32!"
            severity failure;
         wait;
      end process;
   end generate PCHK2;

   PCHK3 : if p(0) /= '1' generate      -- LSB must be 1
      process
      begin
         report "POLYNOMIAL must have lsb set to 1!"
            severity failure;
         wait;
      end process;
   end generate PCHK3;

   CA : for i in 1 to dw generate       -- data bits
      DAT : for j in 1 to msb generate
         da(i)(j) <= data_i(i - 1);
      end generate DAT;
   end generate CA;

   MS0 : for i in 1 to msb generate
      ma(1)(i) <= crc(msb);
   end generate MS0;
   MSP : for i in 2 to dw generate
      MSU : for j in 1 to msb generate
         ma(i)(j) <= crca(i - 1)(msb);
      end generate MSU;
   end generate MSP;

   crca(1)(0)            <= da(1)(1) xor crc(msb);
   crca(1)(msb downto 1) <= crc(msb - 1 downto 0) xor ((da(1) xor ma(1)) and p(msb downto 1));
   FB : for i in 2 to dw generate
      crca(i)(0)            <= da(i)(1) xor crca(i - 1)(msb);
      crca(i)(msb downto 1) <= crca(i - 1)(msb - 1 downto 0) xor
                               ((da(i) xor ma(i)) and p(msb downto 1));
   end generate FB;

   SR : if SYNC_RESET = 1 generate
      srst <= rst_i;
      arst <= '0';
   end generate SR;
   AR : if SYNC_RESET = 0 generate
      srst <= '0';
      arst <= rst_i;
   end generate AR;

   crc_o <= crc;
   zero  <= (others => '0');

   CRCP : process (clk_i, arst)
   begin
      if arst = '1' then                -- async. reset
         crc     <= INIT_VALUE;
         match_o <= '0';
      elsif rising_edge(clk_i) then
         if srst = '1' then             -- sync. reset
            crc     <= INIT_VALUE;
            match_o <= '0';
         elsif clken_i = '1' then
            crc <= crca(dw);
            if crca(dw) = zero then
               match_o <= '1';
            else
               match_o <= '0';
            end if;
         end if;
      end if;
   end process;
   
end rtl;

library ieee;
   use ieee.std_logic_1164.all;
   
library EMBEDDED_LA;

entity UART is
  generic(BRDIVISOR: INTEGER range 0 to 65535 := 130); -- Baud rate divisor
  port (

     WB_CLK_I : in  std_logic;  -- clock
     WB_RST_I : in  std_logic;  -- Reset input
     WB_ADR_I : in  std_logic_vector(1 downto 0); -- Adress bus          
     WB_DAT_I : in  std_logic_vector(7 downto 0); -- DataIn Bus
     WB_DAT_O : out std_logic_vector(7 downto 0); -- DataOut Bus
     WB_WE_I  : in  std_logic;  -- Write Enable
     WB_STB_I : in  std_logic;  -- Strobe
     WB_ACK_O : out std_logic;	-- Acknowledge

     IntTx_O  : out std_logic;  -- Transmit interrupt: indicate waiting for Byte
     IntRx_O  : out std_logic;  -- Receive interrupt: indicate Byte received
     BR_Clk_I : in  std_logic;  -- Clock used for Transmit/Receive
     TxD_PAD_O: out std_logic;  -- Tx RS232 Line
     RxD_PAD_I: in  std_logic);  -- Rx RS232 Line     
end UART;

architecture Behaviour of UART is

  component Counter
  generic(COUNT: INTEGER range 0 to 65535); -- Count revolution
  port (
     Clk      : in  std_logic;  -- Clock
     Reset    : in  std_logic;  -- Reset input
     CE       : in  std_logic;  -- Chip Enable
     O        : out std_logic); -- Output  
  end component;

  component RxUnit
  port (
     Clk    : in  std_logic;  -- system clock signal
     Reset  : in  std_logic;  -- Reset input
     Enable : in  std_logic;  -- Enable input
     ReadA  : in  Std_logic;  -- Async Read Received Byte
     RxD    : in  std_logic;  -- RS-232 data input
     RxAv   : out std_logic;  -- Byte available
     DataO  : out std_logic_vector(7 downto 0)); -- Byte received
  end component;

  component TxUnit
  port (
     Clk    : in  std_logic;  -- Clock signal
     Reset  : in  std_logic;  -- Reset input
     Enable : in  std_logic;  -- Enable input
     LoadA  : in  std_logic;  -- Asynchronous Load
     TxD    : out std_logic;  -- RS-232 data output
     Busy   : out std_logic;  -- Tx Busy
     DataI  : in  std_logic_vector(7 downto 0)); -- Byte to transmit
  end component;

  signal RxData : std_logic_vector(7 downto 0); -- Last Byte received
  signal TxData : std_logic_vector(7 downto 0); -- Last bytes transmitted
  signal SReg   : std_logic_vector(7 downto 0); -- Status register
  signal EnabRx : std_logic;  -- Enable RX unit
  signal EnabTx : std_logic;  -- Enable TX unit
  signal RxAv   : std_logic;  -- Data Received
  signal TxBusy : std_logic;  -- Transmiter Busy
  signal ReadA  : std_logic;  -- Async Read receive buffer
  signal LoadA  : std_logic;  -- Async Load transmit buffer
  signal Sig0   : std_logic;  -- gnd signal
  signal Sig1   : std_logic;  -- vcc signal  
 
  begin
  sig0 <= '0';
  sig1 <= '1';
  Uart_Rxrate : Counter -- Baud Rate adjust
     generic map (COUNT => BRDIVISOR) 
     port map (BR_CLK_I, sig0, sig1, EnabRx); 
  Uart_Txrate : Counter -- 4 Divider for Tx
     generic map (COUNT => 4)  
     port map (BR_CLK_I, Sig0, EnabRx, EnabTx);
  Uart_TxUnit : TxUnit port map (BR_CLK_I, WB_RST_I, EnabTX, LoadA, TxD_PAD_O, TxBusy, TxData);
  Uart_RxUnit : RxUnit port map (BR_CLK_I, WB_RST_I, EnabRX, ReadA, RxD_PAD_I, RxAv, RxData);
  IntTx_O <= not TxBusy;
  IntRx_O <= RxAv;
  SReg(0) <= not TxBusy;
  SReg(1) <= RxAv;
  SReg(7 downto 2) <= "000000";
  
  -- Implements WishBone data exchange.
  -- Clocked on rising edge. Synchronous Reset RST_I
  WBctrl : process(WB_CLK_I, WB_RST_I, WB_STB_I, WB_WE_I, WB_ADR_I)
  variable StatM : std_logic_vector(4 downto 0);
  begin
     if Rising_Edge(WB_CLK_I) then
        if (WB_RST_I = '1') then
  	   ReadA <= '0';
  	   LoadA <= '0';
  	else
	   if (WB_STB_I = '1' and WB_WE_I = '1' and WB_ADR_I = "00") then -- Write Byte to Tx
              TxData <= WB_DAT_I;
	      LoadA <= '1';   -- Load signal
           else LoadA <= '0';	   
           end if;
	   if (WB_STB_I = '1' and WB_WE_I = '0' and WB_ADR_I = "00") then -- Read Byte from Rx
	      ReadA <= '1';   -- Read signal
           else ReadA <= '0';	   
           end if;           
        end if;	
     end if;
  end process;
  WB_ACK_O <= WB_STB_I;
  WB_DAT_O <=
     RxData when WB_ADR_I = "00" else  -- Read Byte from Rx
     SReg when WB_ADR_I = "01" else    -- Read Status Reg
     "00000000";
end Behaviour;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;
    use EMBEDDED_LA.ucrc_pkg.ALL;

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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;
    use EMBEDDED_LA.ucrc_pkg.ALL;

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

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.MATH_REAL.ALL;
    use IEEE.NUMERIC_STD.ALL;    

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.EMBEDDED_LA_PKG.ALL;

entity SAMPLER_MEMORY is
    generic (
        BRAM_COUNT_G : POSITIVE := 4;
        MEM_DATA_WIDTH_G : POSITIVE := 8
    );
    
    port (
        sa_clk_i : in STD_LOGIC;
        mem_i    : in MEM_MOSI_T;
        
        wb_clk_i : in STD_LOGIC;
        wb_rst_i : in STD_LOGIC;
        wb_i     : in WB_MEM_MOSI_T;
        wb_o     : out WB_MEM_MISO_T
    );
end SAMPLER_MEMORY;

architecture INF_MEM of SAMPLER_MEMORY is
    constant WB_MEM_FACTOR_C : integer := 2**INTEGER(ceil(log2(REAL(LA_CFG_MEM_DATA_WIDTH_C)/REAL(LA_CFG_WB_DATA_WIDTH_C))));

    type INF_MEM_T is array(0 to LA_CFG_MEM_MAX_ADDR_C) of STD_LOGIC_VECTOR(LA_CFG_MEM_DATA_WIDTH_C-1 downto 0);
    signal mem_l : INF_MEM_T;
begin
    assert WB_MEM_FACTOR_C = 1 report "SAMPLER_MEMORY supports only LA_CFG_MEM_DATA_WIDTH_C <= LA_CFG_WB_DATA_WIDTH_C" severity failure;
    assert false report "INF_MEM arch of SAMPLER_MEMORY assumes that BRAM-blocks are inferred from memory signal. Please ensure they are" severity note;

 -- write
    write_proc: process(sa_clk_i) is
    begin
        if rising_edge(sa_clk_i) and mem_i.we = '1' then
            mem_l(to_integer(UNSIGNED(mem_i.address))) <= mem_i.data;
        end if;
    end process;
    
 -- read
    read_proc: process(wb_clk_i) is
    begin
        if rising_edge(wb_clk_i) then
            wb_o.data <= (others => '0');
            wb_o.data <= mem_l(to_integer(UNSIGNED(wb_i.address)));
        end if;
    end process;
    
    wb_o.ack <= wb_i.stb;
end INF_MEM;

library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.LOGIC_ANALYZER_PKG.ALL;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    
library EMBEDDED_LA;
    use EMBEDDED_LA.RS232_CONTROLLER_PKG.ALL;

library EMBEDDED_LA;
    use EMBEDDED_LA.CONFIG_PKG.ALL;
    use EMBEDDED_LA.EMBEDDED_LA_PKG.ALL;

entity EMBEDDED_LA_CORE is
    port(
        clk_i       : in  STD_LOGIC;
        
        sa_data_i   : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C * LA_CFG_BANK_COUNT_C - 1 downto 0);
        trg_data_i  : in  STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C - 1 downto 0);

    -- RS232
        rxd_i       : in  STD_LOGIC;
        txd_o       : out STD_LOGIC
    );
end EMBEDDED_LA_CORE;

architecture Behavioral of EMBEDDED_LA_CORE is

    signal rst_l : INTEGER range 0 to 255 := 0;

    signal wb_rst_l : STD_LOGIC := '1';
    signal wb_miso_l : WB_MISO_T;
    signal wb_mosi_l : WB_MOSI_T;

 -- SAMPLER MEMORY
    signal mem_sa_clk_l : STD_LOGIC;
    signal mem_mosi_l : MEM_MOSI_T;
    signal mem_wb_mosi_l : WB_MEM_MOSI_T;
    signal mem_wb_miso_l : WB_MEM_MISO_T;
begin
    my_la : LOGIC_ANALYZER_CORE
    port map(
        sa_clk_i => clk_i,
        sa_data_i => sa_data_i,

        trg_data_i => trg_data_i,
        --trg_o => ,
        
        --mem_state_o => ,
        mem_clk_o => mem_sa_clk_l,
        mem_o => mem_mosi_l,
        mem_wb_i => mem_wb_miso_l,
        mem_wb_o => mem_wb_mosi_l,
        
        wb_clk_i => clk_i,
        wb_rst_i => wb_rst_l,
        wb_o => wb_miso_l,
        wb_i => wb_mosi_l
    );
    

    my_mem : SAMPLER_MEMORY
    generic map (
        bram_count_g => 8
    )
    port map (
        sa_clk_i => mem_sa_clk_l,
        mem_i => mem_mosi_l,
        wb_clk_i => clk_i,
        wb_rst_i => wb_rst_l,
        wb_i => mem_wb_mosi_l,
        wb_o => mem_wb_miso_l
    );    
    

    my_rs232 : RS232_CONTROLLER_CORE
    generic map (
        uart_br_divisor => LA_CFG_RS232_DIV
    )
    port map (
        txd_pad_o => txd_o,
        rxd_pad_i => rxd_i,
        
        wb_clk_i => clk_i,
        wb_rst_i => wb_rst_l,
        wb_i => wb_miso_l,
        wb_o => wb_mosi_l
   );

    proc_reset: process(clk_i) is
    begin
        if rising_edge(clk_i) then
            if rst_l < 255 then
                rst_l <= rst_l + 1;
            else
                wb_rst_l <= '0';
            end if;
        end if;
    end process;
end Behavioral;

