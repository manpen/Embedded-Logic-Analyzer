---------------------------------------------------------------------------------------------------
-- Project:
--   Logic Analyzer (Bachelor Thesis)
--   Manuel Penschuck - manuel at penschuck dot eu
--   Goethe Universitaet Frankfurt/Main, Deutschland
--   Institut fuer Informatik, Professur fuer Eingebette Systeme
--
-- Create Date:
--   13.02.2012
--
-- Description:
--   This module describes a minimal logic anylzer with an RS232 interface
--   and some block ram. It is designed to be used with a XILINX SPARTAN 3,
--   but should be synthesizable for virtually any plattform. It functions
--   as a embeddable logic analyzer, that exports only the necessary ports.
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

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    
library RS232_CONTROLLER;
    use RS232_CONTROLLER.RS232_CONTROLLER_PKG.ALL;

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
-- RESET
    signal rst_l : INTEGER range 0 to 255 := 0;

-- WISHBONE
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
    
-- SAMPLER MEMORY
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
    
-- RS232
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

