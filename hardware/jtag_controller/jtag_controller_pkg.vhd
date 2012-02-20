library IEEE;
    use IEEE.STD_LOGIC_1164.all;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    
library JTAG_CONTROLLER;

package JTAG_CONTROLLER_PKG is
    type JTAG_STATE_T is record
        reset : STD_LOGIC;
        capture : STD_LOGIC;
        shift : STD_LOGIC;
        update : STD_LOGIC;
    end record;
        
    type JTAG_REGISTER_MOSI_T is record
        clk : STD_LOGIC;
        tdi : STD_LOGIC;
        sel : STD_LOGIC;
    end record;
    
    component JTAG_CONTROLLER_CORE is
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
    end component;
end JTAG_CONTROLLER_PKG;

package body JTAG_CONTROLLER_PKG is
end JTAG_CONTROLLER_PKG;