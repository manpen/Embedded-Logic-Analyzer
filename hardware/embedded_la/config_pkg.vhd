library IEEE;
    use IEEE.STD_LOGIC_1164.all;
    use IEEE.MATH_REAL.all;

library EMBEDDED_LA;

package config_pkg is
-- SAMPLING DATA CONFIG
    type INPUT_MUX_T is (ISR, MUX);
    CONSTANT LA_CFG_BANK_WIDTH_C : INTEGER range 1 to 255 := 8;
    CONSTANT LA_CFG_BANK_COUNT_C : INTEGER range 1 to 255 := 1;
    CONSTANT LA_CFG_MUX_METHOD_C : INPUT_MUX_T := MUX;

-- WISHBONE CONFIG I (continued at bottom)
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
    
-- RS232
    CONSTANT LA_CFG_CLK_FRQ : POSITIVE := 50_000_000;
    CONSTANT LA_CFG_RS232_BAUD : POSITIVE := 115_200;
    CONSTANT LA_CFG_RS232_DIV : POSITIVE :=
      INTEGER(round(real(LA_CFG_CLK_FRQ) / real(LA_CFG_RS232_BAUD) / 4.0));

-- MEM CONFIG
    CONSTANT LA_CFG_MEM_DATA_WIDTH_C : INTEGER := 8;
    CONSTANT LA_CFG_MEM_MAX_ADDR_C : INTEGER := 255; --16383;
    CONSTANT LA_CFG_MEM_ADDR_WIDTH_C : INTEGER := 14;

-- ENCODING CONFIG
    CONSTANT LA_CFG_RLE_C : BOOLEAN := FALSE; -- include run-length encoding support

-- TRIGGER
    CONSTANT LA_CFG_TRG_WIDTH_C : INTEGER RANGE 1 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 8;
    CONSTANT LA_CFG_EDGE_TRIGGERS_C : INTEGER RANGE 0 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 2;
    CONSTANT LA_CFG_VALUE_TRIGGERS_C : INTEGER RANGE 0 TO 2**LA_CFG_WB_DATA_WIDTH_C - 1 := 2;

-- MISC
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
 
