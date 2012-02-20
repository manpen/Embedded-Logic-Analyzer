---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   18.08.2011
--
-- Description:
--   This module describes a minimal logic anylzer with an RS232 interface
--   and some block ram. It is designed to be used with a XILINX SPARTAN 3.
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
    use IEEE.STD_LOGIC_UNSIGNED.ALL;
    use IEEE.NUMERIC_STD.ALL;
    use IEEE.MATH_REAL.ALL;

    
library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    
library RS232_CONTROLLER;
    use RS232_CONTROLLER.RS232_CONTROLLER_PKG.ALL;

library JTAG_CONTROLLER;
   use JTAG_CONTROLLER.JTAG_CONTROLLER_PKG.ALL;

library unisim;
    use unisim.vcomponents.all;

library SOC_SPARTAN3;
    use SOC_SPARTAN3.SOC_SPARTAN3_PKG.ALL;

entity soc is
    port (
        sys_clk_i   : in  STD_LOGIC;
    
    -- DIRECT USER INTERACTION
        buttons_i   : in  STD_LOGIC_VECTOR(3 downto 0);
        switches_i  : in  STD_LOGIC_VECTOR(7 downto 0);
        leds_o      : out STD_LOGIC_VECTOR(7 downto 0);
        segment7_o  : out STD_LOGIC_VECTOR(10 downto 0);
        
    -- RS232
        rxd_i       : in  STD_LOGIC;
        txd_o       : out STD_LOGIC
    );
end soc;

architecture Behavioral of soc is
 -- LOGIC ANALYZER
    signal la_sa_clk_l : STD_LOGIC;
    signal la_data_pattern_l : STD_LOGIC_VECTOR(10 downto 0);
    signal la_sa_data_l : STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C * LA_CFG_BANK_COUNT_C - 1 downto 0);
    signal la_mem_state_l : MEM_STATE_T;
    
    signal la_trg_l : STD_LOGIC;
    
-- WISHBONE
    signal wb_clk_l, wb_rst_l : STD_LOGIC;
    signal wb_miso_l, wb_test_miso_l : WB_MISO_T;
    signal wb_mosi_l, wb_test_mosi_l : WB_MOSI_T;

 -- SAMPLER MEMORY
    signal mem_sa_clk_l : STD_LOGIC;
    signal mem_mosi_l : MEM_MOSI_T;
    signal mem_wb_mosi_l : WB_MEM_MOSI_T;
    signal mem_wb_miso_l : WB_MEM_MISO_T;
    
 -- SEVEN SEGMENT
    signal seg7_data_l    : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0');
    signal seg7_segment_l : STD_LOGIC_VECTOR(6 downto 0);
    signal seg7_digit_l   : STD_LOGIC_VECTOR(3 downto 0);

 -- JTAG
    signal jtag_state_l : JTAG_STATE_T;
    signal jtag_op_reg_l : JTAG_REGISTER_MOSI_T;
    signal jtag_op_reg_tdo_l : STD_LOGIC;
    signal jtag_data_reg_l : JTAG_REGISTER_MOSI_T;
    signal jtag_data_reg_tdo_l : STD_LOGIC;
    signal jtag_tdi_l : STD_LOGIC;
    
    component BSCAN_SPARTAN3
       port (CAPTURE : out STD_ULOGIC;
             DRCK1 : out STD_ULOGIC;
             DRCK2 : out STD_ULOGIC;
             RESET : out STD_ULOGIC;
             SEL1 : out STD_ULOGIC;
             SEL2 : out STD_ULOGIC;
             SHIFT : out STD_ULOGIC;
             TDI : out STD_ULOGIC;
             UPDATE : out STD_ULOGIC;
             TDO1 : in STD_ULOGIC;
             TDO2 : in STD_ULOGIC);
    end component;     

 -- MISC
    signal buttons_deb_l    : STD_LOGIC_VECTOR(buttons_i'RANGE);
    signal clocks_l : STD_LOGIC_VECTOR(20 downto 0);
    signal s7_data_miso_l, s7_data_mosi_l, s7_addr_l : STD_LOGIC_VECTOR(seg7_data_l'RANGE) := (others => '0');
begin
    s7_data_miso_l(wb_miso_l.data'RANGE) <= wb_miso_l.data;
    s7_data_mosi_l(wb_mosi_l.data'RANGE) <= wb_mosi_l.data;
    s7_addr_l(wb_mosi_l.address'RANGE) <= wb_mosi_l.address;
   
    seg7_data_l <= 
        s7_addr_l when switches_i(1 downto 0) = "00" else
        s7_data_miso_l when switches_i(1 downto 0) = "01" else
        s7_data_mosi_l when switches_i(1 downto 0) = "10" else
        s7_addr_l(7 downto 0) & s7_data_miso_l(7 downto 0);

---- test setup
--    
    leds_o <= (0 => active_high_c(la_mem_state_l = EMPTY),
               1 => active_high_c(la_mem_state_l = CAPTURING),
               2 => active_high_c(la_mem_state_l = FULL),
               
               6 => la_trg_l,
               7 => wb_rst_l,
               others => '0');
--
---- div clocks
--    process(sys_clk_i) is
--    begin
--        if rising_edge(sys_clk_i) then
--            clocks_l <= clocks_l + STD_LOGIC_VECTOR(TO_UNSIGNED(1, clocks_l'LENGTH));
--        end if;
--    end process;
--
---- DEBOUNCE BUTTONS
    my_deb : DEBOUNCE 
        generic map(4, 16)
        port map (clk_i => sys_clk_i, data_i => buttons_i, data_o => buttons_deb_l);
--    
-- LOGIC ANALYZER
   -- wb_rst_l <= '0';
    
    la_data_pattern_l <= buttons_i(2 downto 0) & switches_i;
    my_data: for i in 0 to integer(ceil(real(la_sa_data_l'LENGTH) / real(la_data_pattern_l'LENGTH))) - 1 generate
        la_sa_data_l(min_f(la_sa_data_l'HIGH, (i+1)*la_data_pattern_l'LENGTH - 1) downto i*la_data_pattern_l'LENGTH)
            <= la_data_pattern_l(min_f(la_sa_data_l'HIGH, (i+1)*la_data_pattern_l'LENGTH - 1) - i*la_data_pattern_l'LENGTH downto 0);
    end generate;

    wb_clk_l <= sys_clk_i;

    my_la : LOGIC_ANALYZER_CORE
    port map(
        sa_clk_i => sys_clk_i,
        sa_data_i => la_sa_data_l,

        trg_data_i => la_sa_data_l(LA_CFG_TRG_WIDTH_C-1 downto 0),
        trg_o => la_trg_l,
        
        mem_state_o => la_mem_state_l,
        mem_clk_o => mem_sa_clk_l,
        mem_o => mem_mosi_l,
        mem_wb_i => mem_wb_miso_l,
        mem_wb_o => mem_wb_mosi_l,
        
        wb_clk_i => wb_clk_l,
        wb_rst_i => wb_rst_l,
        wb_o => wb_miso_l,
        wb_i => wb_mosi_l
    );
    
-- SAMPLER MEMORY
    my_mem : SAMPLER_MEMORY
    generic map (
        bram_count_g => 8
    )
    port map (
        sa_clk_i => mem_sa_clk_l,
        mem_i => mem_mosi_l,
        wb_clk_i => wb_clk_l,
        wb_rst_i => wb_rst_l,
        wb_i => mem_wb_mosi_l,
        wb_o => mem_wb_miso_l
    );    
    
-- RS232
    my_rs232 : RS232_CONTROLLER_CORE
    generic map (
        uart_br_divisor => 109 -- 110 KBps
    )
    port map (
        txd_pad_o => txd_o,
        rxd_pad_i => rxd_i,
        
        wb_clk_i => wb_clk_l,
        wb_rst_i => wb_rst_l,
        wb_i => wb_miso_l,
        wb_o => wb_mosi_l
       -- wb_i => wb_test_miso_l,
       -- wb_o => wb_test_mosi_l      
   );
   
---- JTAG
--    my_jtag : JTAG_CONTROLLER_CORE
--    port map (
--        jtag_state_i => jtag_state_l,
--        jtag_op_reg_i => jtag_op_reg_l,
--        jtag_op_reg_tdo_o => jtag_op_reg_tdo_l,
--        jtag_data_reg_i => jtag_data_reg_l,
--        jtag_data_reg_tdo_o => jtag_data_reg_tdo_l,
--
--        wb_clk_i => wb_clk_l,
--        wb_rst_i => wb_rst_l,
--        --wb_i => wb_test_miso_l,
--        --wb_o => wb_test_mosi_l      
--        wb_i => wb_miso_l,
--        wb_o => wb_mosi_l
--   );   
----   
--   
---- BSCAN
--    my_bscan: BSCAN_SPARTAN3 
--    port map (
--        reset =>   jtag_state_l.reset,
--        capture => jtag_state_l.capture,
--        shift =>   jtag_state_l.shift,
--        update =>  jtag_state_l.update,
--
--        tdi => jtag_tdi_l,
--
--        drck1 => jtag_op_reg_l.clk,
--        sel1 =>  jtag_op_reg_l.sel,
--        tdo1 =>  jtag_op_reg_tdo_l,
--
--        drck2 => jtag_data_reg_l.clk,
--        sel2 =>  jtag_data_reg_l.sel,
--        tdo2 =>  jtag_data_reg_tdo_l
--    );    
--    jtag_op_reg_l.tdi <= jtag_tdi_l;
--    jtag_data_reg_l.tdi <= jtag_tdi_l;
--
--
--   mem: RAMB16_S9
--   generic map (
--    INIT_00 => X"1F1E1D1C1B1A191817161514131211100F0E0D0C0B0A09080706050403020100",
--    INIT_01 => X"3F3E3D3C3B3A393837363534333231302F2E2D2C2B2A29282726252423222120"
--   )
--   port map (
--        we => wb_test_mosi_l.we,
--        en => '1',
--        ssr => '0',
--        dip => "0",
--        clk => wb_clk_l,
--        addr => wb_test_mosi_l.address(10 downto 0),
--        di => wb_test_mosi_l.data(7 downto 0),
--        do => wb_test_miso_l.data(7 downto 0)
--   );
--   
--   wb_test_miso_l.data(wb_test_miso_l.data'HIGH downto 8) <= (others => '0');
--   wb_test_miso_l.ack <= wb_test_mosi_l.stb;
--   
   


-- SEVEN SEGMENT DRIVER
    my_seg7 : SEGMENT7
    generic map (clk_divisor_g => 1000)
    port map (
        clk_i => sys_clk_i,
        data_i => seg7_data_l,
        segment_o => seg7_segment_l,
        digit_o => seg7_digit_l
    );
    segment7_o <= seg7_digit_l & seg7_segment_l;
end Behavioral;

