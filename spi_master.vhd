LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_arith.all;
USE ieee.std_logic_unsigned.all;

ENTITY spi_master IS
  GENERIC(
    d_width : INTEGER := 8);  -- lungimea pachetului de biti
  PORT(
    clock   : IN     STD_LOGIC;                             
    reset   : IN     STD_LOGIC;                             
    enable  : IN     STD_LOGIC;  
    cpha    : IN     STD_LOGIC;                             --spi clock phase                           
    cont    : IN     STD_LOGIC;                             
    clk_div : IN     INTEGER;                               --1/2 din clock-ul spi
    tx_data : IN     STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  
    miso    : IN     STD_LOGIC;                             --master in, slave out
    sclk    : INOUT  STD_LOGIC;                             --spi clock
    ss      : INOUT  STD_LOGIC;                             --slave select
    mosi    : OUT    STD_LOGIC;                             --master out, slave in
    busy    : OUT    STD_LOGIC;                             
    rx_data : OUT    STD_LOGIC_VECTOR(d_width-1 DOWNTO 0)); --pachetul de biti primiti
END spi_master;

ARCHITECTURE logic OF spi_master IS
  TYPE state_type IS(ready, execute);                           
  SIGNAL state       : state_type;                              --starea curenta
  SIGNAL clk_ratio   : INTEGER;                              
  SIGNAL count       : INTEGER;                              
  SIGNAL clk_toggles : INTEGER RANGE 0 TO d_width*2 + 1;     
  SIGNAL assert_data : STD_LOGIC  ;                       --'1' pentru tx sclk toggle, '0' pentru rx sclk toggle
  SIGNAL continue    : STD_LOGIC;                               
  SIGNAL rx_buffer   : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);    --buffer pentru primirea de date
  SIGNAL tx_buffer   : STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);    --buffer pentru trimiterea de date
  SIGNAL last_bit_rx : INTEGER RANGE 0 TO d_width*2;            --ultimul bit din rx
BEGIN
  PROCESS(clock, reset)
  BEGIN

    IF(reset = '0') THEN        
      busy <= '1';                
      ss <= '1';                  --slave-ul curent
      mosi <= 'Z';                --impedanta inalta pe mosi
      rx_data <= (OTHERS => '0'); 
      state <= ready;             

    ELSIF(clock'EVENT AND clock = '1') THEN
      CASE state IS               

        WHEN ready =>
          busy <= '0';             
          ss <= '1';               
          mosi <= 'Z';             
          continue <= '0';         

          --comanda trimisa de catre user
          IF(enable = '1') THEN       
            busy <= '1';             
            IF(clk_div = 0) THEN     
              clk_ratio <= 1;        
              count <= 1;            
            ELSE
              clk_ratio <= clk_div;  
              count <= clk_div;      
            END IF;
            assert_data <= NOT cpha;
            tx_buffer <= tx_data;    
            clk_toggles <= 0;        
            last_bit_rx <= d_width*2 + conv_integer(cpha) - 1; 
            state <= execute;        
          ELSE
            state <= ready;          
          END IF;

        WHEN execute =>
          busy <= '1';        
          ss <= '0'; 
          IF(count = clk_ratio) THEN        
            count <= 1;                     
            assert_data <= NOT assert_data; --se schimba indicatorul de receive/transmite
            IF(clk_toggles = d_width*2 + 1) THEN
              clk_toggles <= 0;               
            ELSE
              clk_toggles <= clk_toggles + 1; 
            END IF;
            
            IF(clk_toggles <= d_width*2 AND ss = '0') THEN 
              sclk <= NOT sclk;                   -- SPI clock
            END IF;
            
            IF(assert_data = '0' AND clk_toggles < last_bit_rx + 1 AND ss = '0') THEN 
              rx_buffer <= rx_buffer(d_width-2 DOWNTO 0) & miso;          --Primirea de biti
            END IF;

            IF(assert_data = '1' AND clk_toggles < last_bit_rx) THEN 
              mosi <= tx_buffer(d_width-1);                     
              tx_buffer <= tx_buffer(d_width-2 DOWNTO 0) & '0';
            END IF;
            
            
            IF(clk_toggles = last_bit_rx AND cont = '1') THEN 
              tx_buffer <= tx_data;                       
              clk_toggles <= last_bit_rx - d_width*2 + 1; 
              continue <= '1';                            
            END IF;
            
            IF(continue = '1') THEN  
              continue <= '0';      
              busy <= '0';          --Transmission
              rx_data <= rx_buffer;   
            END IF;
            
            IF((clk_toggles = d_width*2 + 1) AND cont = '0') THEN   
              busy <= '0';             
              ss <= '1'; 
              mosi <= 'Z';             
              rx_data <= rx_buffer;    
              state <= ready;          
            ELSE                       
              state <= execute;        
            END IF;
          
          ELSE        
            count <= count + 1; 
            state <= execute;   
          END IF;

      END CASE;
    END IF;
  END PROCESS; 
END logic;
