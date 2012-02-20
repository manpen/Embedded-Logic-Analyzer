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
--   Some helper functions to ease the implementation of the wishbone interface.
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
    use LOGIC_ANALYZER.CONFIG_PKG.ALL;
    use LOGIC_ANALYZER.LOGIC_ANALYZER_PKG.ALL;

package WISHBONE is
-- SIGNAL
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

-- VARIABLE
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
                                      
-- CONSTANT
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

-- MISC. HELPER
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

-- SIGNAL
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

-- VARIABLE
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

-- CONSTANT
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