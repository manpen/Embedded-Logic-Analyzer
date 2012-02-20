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
--   This file is a collection of helper functions and procedures used in the 
--   logic analyzer. Futhermore it contains the component descriptions of the
--   logic analyzer core. It is sufficient to use this package in order to
--   instantiate the analyzer.
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
    use IEEE.STD_LOGIC_1164.all;
    
library LOGIC_ANALYZER;
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;

package LOGIC_ANALYZER_PKG is
-- CONFIGURATION TYPES AND HELPER
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
