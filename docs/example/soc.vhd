library IEEE;
    use IEEE.STD_LOGIC_1164.ALL;
    use IEEE.NUMERIC_STD.ALL;    

library EMBEDDED_LA;
    use EMBEDDED_LA.EMBEDDED_LA_PKG.ALL;

entity SOC is
    port (
        clk_i  : in STD_LOGIC;
        leds_o : out STD_LOGIC_VECTOR(7 downto 0);
    
    -- rs232
        rxd_i : in  STD_LOGIC;
        txd_o : out STD_LOGIC
    );
end SOC;

architecture Behavioral of SOC is
    signal demo_counter_l : STD_LOGIC_VECTOR(25 downto 0);
begin
-- demo application
    counter_proc: process(clk_i) is
    begin
        if rising_edge(clk_i) then
            demo_counter_l <= STD_LOGIC_VECTOR(UNSIGNED(demo_counter_l) + to_unsigned(1,1));
        end if;
    end process;
    leds_o(7 downto 0) <= demo_counter_l(demo_counter_l'HIGH downto demo_counter_l'HIGH-7);

-- observe with LA
    my_la : EMBEDDED_LA_CORE port map (
        clk_i => clk_i,
        sa_data_i => demo_counter_l(7 downto 0),
        trg_data_i => demo_counter_l(7 downto 0),
        rxd_i => rxd_i,
        txd_o => txd_o
    );
end Behavioral;

