LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY accelerometer_adxl362 IS
  GENERIC(
    clk_freq   : INTEGER := 100;              --in MHz
    data_rate  : STD_LOGIC_VECTOR := "011";  --data rate pentru acelerometru
    data_range : STD_LOGIC_VECTOR := "11");  --data range penru acelerometru
  PORT(
    clk            : IN  STD_LOGIC;                      
    tx_ena         : IN  STD_LOGIC;
    rx             : IN  STD_LOGIC;
    tx             : OUT STD_LOGIC; 
    reset          : IN  STD_LOGIC;                      
    miso           : IN     STD_LOGIC;                      
    sclk           : INOUT  STD_LOGIC;                      
    ss             : INOUT  STD_LOGIC;   
    mosi           : OUT    STD_LOGIC);                      
END accelerometer_adxl362;

ARCHITECTURE behavior OF accelerometer_adxl362 IS
  TYPE state_type IS(start, pause, configure, read_data, output_result); 
  
  SIGNAL rx_data  :   STD_LOGIC_VECTOR(23 DOWNTO 0); --semnale UART
  SIGNAL tx_data  :   STD_LOGIC_VECTOR(23 DOWNTO 0);
   
  SIGNAL state              : state_type := start;                      
  SIGNAL parameter          : INTEGER RANGE 0 TO 3;                   --parametrul de configurare
  SIGNAL parameter_addr     : STD_LOGIC_VECTOR(5 DOWNTO 0);           --adresa parametrului de configurare a SPI
  SIGNAL parameter_data     : STD_LOGIC_VECTOR(7 DOWNTO 0);           --valoarea parametrului de configurarea a SPI
  SIGNAL spi_busy_prev      : STD_LOGIC;                              --starea anterioara a spi_busy
  SIGNAL spi_busy           : STD_LOGIC;                              --flag pentru SPI cand lucreaza
  SIGNAL spi_ena            : STD_LOGIC;                              
  SIGNAL spi_cont           : STD_LOGIC;                              --mod continuu SPI
  SIGNAL spi_tx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --pachetul de date trimis la SPI
  SIGNAL spi_rx_data        : STD_LOGIC_VECTOR(7 DOWNTO 0);           --pachetul de date primit de la SPI
  SIGNAL acceleration_x_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          
  SIGNAL acceleration_y_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          
  SIGNAL acceleration_z_int : STD_LOGIC_VECTOR(15 DOWNTO 0);          
BEGIN

    UART: entity Work.uart port map(clk => clk, reset =>reset,tx_ena => tx_ena, tx_data => tx_data, rx => rx, rx_data => rx_data, tx=>tx); 
    SPI: entity Work.spi_master GENERIC MAP( d_width => 8)
    PORT MAP(clock => clk, reset => reset, enable => spi_ena,cpha => '0',
          cont => spi_cont, clk_div => clk_freq/16, tx_data => spi_tx_data, miso => miso,
          sclk => sclk, ss => ss, mosi => mosi, busy => spi_busy, rx_data => spi_rx_data);

  PROCESS(clk)
    VARIABLE count : INTEGER := 0; 
  BEGIN
    IF(reset = '0') THEN              
      spi_busy_prev <= '0';               
      spi_ena <= '0';                     
      spi_cont <= '0';                    
      spi_tx_data <= (OTHERS => '0');     
      state <= start;
      tx_data<=(OTHERS =>'0');                    
    ELSIF(clk'EVENT AND clk = '1') THEN 
      CASE state IS                       

        WHEN start =>
          count := 0;      
          parameter <= 0; 
          state <= pause;
          
        WHEN pause =>
          IF(spi_busy = '0') THEN                
            IF(count < clk_freq/5) THEN            -- pauza de 200ns
              count := count + 1;                   
              state <= pause;                        
            ELSE                                   
              count := 0;                            
              CASE parameter IS                      --case pentru selectarea tranzactiei
                WHEN 0 =>                            --setarea range-ului si ratei de transfer
                  parameter <= parameter + 1;            
                  parameter_addr <= "101100";            
                  parameter_data <= data_range & "010" & data_rate;  
                  state <= configure;                    
                WHEN 1 =>                             --case pentru setaraea parametrilor de citire
                  parameter <= parameter + 1;           
                  parameter_addr <= "101101";            
                  parameter_data <= "00000010";          --comanda pentru initierea citirii
                  state <= configure;                    
                WHEN 2 =>                              --case pentru inceperea efectiva a masuratorii
                  state <= read_data;                   
                WHEN OTHERS => NULL;
              END CASE;        
            END IF;
          END IF;
 
        WHEN configure =>  -- primele date trimise pentru configurarea acelerometrului
          spi_busy_prev <= spi_busy;                      
          IF(spi_busy_prev = '1' AND spi_busy = '0') THEN --trecere din 1 in 0
            count := count + 1;                             
          END IF;
          CASE count IS                                   --numarul de schimbari ale flag-ului spi_busy
            WHEN 0 =>                                       --nicio schimbare
              IF(spi_busy = '0') THEN                         --tranzactie neinceputa
                spi_cont <= '1';                                --setarea pe mod continuu
                spi_ena <= '1';                                 
                spi_tx_data <= "00001010";                      --write command
              ELSE                                            --inceperea tranzactiei
                spi_tx_data <= "00" & parameter_addr;           --trimiterea adresei parametrului
              END IF;
            WHEN 1 =>                                         --prima schimbare
              spi_tx_data <= parameter_data;                    --trimiterea comenzii de citire
            WHEN 2 =>                                         --a doua schimbare                                      
              spi_cont <= '0';                                --clear la modul continuu
              spi_ena <= '0';                                 --oprirea spi
              count := 0;                                     
              state <= pause;                                 
            WHEN OTHERS => NULL;
          END CASE;

        WHEN read_data =>
          spi_busy_prev <= spi_busy;                        
          IF(spi_busy_prev = '1' AND spi_busy = '0') THEN   --tranzitie
            count := count + 1;                              
          END IF;          
          CASE count IS                                    
            WHEN 0 =>                                         --nicio schimbare
              IF(spi_busy = '0') THEN                           
                spi_cont <= '1';                                  --setare pe mod continuu
                spi_ena <= '1';                                   
                spi_tx_data <= "00001011";                        --read command
              ELSE                                              --inceperea tranzactiei
                spi_tx_data <= "00001110";                        --register addres
              END IF;
            WHEN 1 =>                                         --prima schimbare
              spi_tx_data <= "00000000";                        
            WHEN 3 =>                                         
              acceleration_x_int(7 DOWNTO 0) <= spi_rx_data;    --primul byte pentru axa x
            WHEN 4 =>                                         
              acceleration_x_int(15 DOWNTO 8) <= spi_rx_data;   --al doilea byte pentru axa x
            WHEN 5 =>                                         
              acceleration_y_int(7 DOWNTO 0) <= spi_rx_data;    --primul byte pentru axa y
            WHEN 6 =>                                         
              acceleration_y_int(15 DOWNTO 8) <= spi_rx_data;   --al doilea byte pentru axa y
            WHEN 7 =>                                         
              spi_cont <= '0';                                  --se opreste modul continuu
              spi_ena <= '0';                                   
              acceleration_z_int(7 DOWNTO 0) <= spi_rx_data;    --primul byte pentru axa z
            WHEN 8 =>                                         
              acceleration_z_int(15 DOWNTO 8) <= spi_rx_data;   --al doilea byte pentru axa z
              count := 0;                                       
              state <= output_result;                          
            WHEN OTHERS => NULL;
          END CASE;
  
        WHEN output_result =>
            tx_data <= acceleration_x_int(11 downto 4) & acceleration_y_int(11 downto 4) & acceleration_z_int(11 downto 4);-- se trimite in buffer-ul de scriere pentru UART
            state <= pause;                                     
        
        WHEN OTHERS => 
          state <= start;

      END CASE;      
    END IF;
  END PROCESS;
END behavior;
