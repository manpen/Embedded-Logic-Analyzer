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
--   Spartan3 reference implementation of the logic analyzer framework
--
--   This Package defines all commonly used hardware depenedent components.
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
    use IEEE.STD_LOGIC_1164.all;

library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;

library SOC_SPARTAN3;

package SOC_SPARTAN3_PKG is
    type BOOL_TO_LOGIC_T is array(BOOLEAN) of STD_LOGIC;
    constant active_high_c : BOOL_TO_LOGIC_T := (false => '0', true => '1');

	component la_embedding is
		port(
			clk_i       : in  STD_LOGIC;
			
			sa_data_i   : in  STD_LOGIC_VECTOR(LA_CFG_BANK_WIDTH_C * LA_CFG_BANK_COUNT_C - 1 downto 0);
			trg_data_i  : in  STD_LOGIC_VECTOR(LA_CFG_TRG_WIDTH_C - 1 downto 0);
	
		-- RS232
			rxd_i       : in  STD_LOGIC;
			txd_o       : out STD_LOGIC
		);
	end component;

    component DEBOUNCE is
        generic (
            width_g : NATURAL;
            deb_length_g : POSITIVE := 8
        );
        port (
            clk_i : in  STD_LOGIC;
            data_i : in  STD_LOGIC_VECTOR(width_g - 1 downto 0);
            data_o : out  STD_LOGIC_VECTOR(width_g - 1 downto 0)
        );
    end component;

    component segment7 is
        generic (
            clk_divisor_g : POSITIVE := 1
        );
        port (
            clk_i  : in  STD_LOGIC;
            data_i : in  STD_LOGIC_VECTOR (15 downto 0);
            
            segment_o : out STD_LOGIC_VECTOR (6 downto 0);
            digit_o   : out STD_LOGIC_VECTOR (3 downto 0)
        );
    end component segment7;
    
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
end SOC_SPARTAN3_PKG;

package body SOC_SPARTAN3_PKG is
end SOC_SPARTAN3_PKG;