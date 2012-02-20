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
--   This module defines a double port memory entity with a variable 
--   address and data width. One port is write-only, the other one read-only.
--   
--   The architecture provided supports only a fixed data width, namely
--   [mem_data_width_g] = 32.
--  
--   Therefore the address width has to be atleast
--   [LA_CFG_MEM_ADDR_WIDTH_C] = 9 + ceil(log([bram_count]))
--
--   Furthermore the archticture uses Xilinx Spartan 3 BRAM-Blocks, rendering
--   it device-dependend.
--
--   This limitations may be overcome by implementing a new architecture.
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
    use IEEE.MATH_REAL.ALL;
    use IEEE.NUMERIC_STD.ALL;    

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;

library SOC_SPARTAN3;
    use SOC_SPARTAN3.SOC_SPARTAN3_PKG.ALL;

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