LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY uart IS
  GENERIC(
    clk_freq  :  INTEGER    := 50_000_000;  --frecventa clock
    baud_rate :  INTEGER    := 19_200;      --rata de transfer
    d_width   :  INTEGER    := 24);        --dimensiune data
  PORT(
    clk      :  IN   STD_LOGIC;                            
    reset  :  IN   STD_LOGIC;                              
    tx_ena   :  IN   STD_LOGIC;                             
    tx_data  :  IN   STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  
    rx       :  IN   STD_LOGIC;                             
    rx_data  :  OUT  STD_LOGIC_VECTOR(d_width-1 DOWNTO 0);  
    tx       :  OUT  STD_LOGIC);                            
END uart;
    
ARCHITECTURE logic OF uart IS
  TYPE   tx_state_type IS (idle, transmit);                       --tip pentru transmisie de date
  TYPE   rx_state_type IS (idle, receive);                        --tip pentru primire de date
  SIGNAL tx_state     :  tx_state_type;                          
  SIGNAL rx_state     :  rx_state_type;                          
  SIGNAL baud_pulse   :  STD_LOGIC := '0';                    --puls la fiecare d_with biti transmisi
  SIGNAL rx_buffer    :  STD_LOGIC_VECTOR(d_width DOWNTO 0) := (OTHERS => '0');   --buffer de primire
  SIGNAL tx_buffer    :  STD_LOGIC_VECTOR(d_width+1 DOWNTO 0) := (OTHERS => '1'); --buffer de trimitere
BEGIN

  --generare baud pulse
  PROCESS(reset, clk)
    VARIABLE count_baud :  INTEGER RANGE 0 TO clk_freq/baud_rate-1 := 0;         --perioada de baud
  BEGIN
    IF(reset = '0') THEN                            
      baud_pulse <= '0';                               
      count_baud := 0;                                
    ELSIF(clk'EVENT AND clk = '1') THEN
      IF(count_baud < clk_freq/baud_rate-1) THEN        --nu s-a ajuns la perioada de baud
        count_baud := count_baud + 1;                     
        baud_pulse <= '0';                               
      ELSE                                              --s-a atins perioada de baud
        count_baud := 0;                                  
        baud_pulse <= '1';                               
      END IF;
    END IF;
  END PROCESS;

  --state machine pentru rx
  PROCESS(reset, clk)
    VARIABLE rx_count :  INTEGER RANGE 0 TO d_width+2 := 0; --numara bitii primiti
  BEGIN
    IF(reset = '0') THEN                                
      rx_count := 0;                                         
      rx_data <= (OTHERS => '0');                            
      rx_state <= idle;                                      
    ELSIF(clk'EVENT AND clk = '1') THEN 
      CASE rx_state IS
      
        WHEN idle =>                                           
          IF(rx = '0') THEN                                                                            
              rx_count := 0;                                        
              rx_buffer <= rx & rx_buffer(d_width DOWNTO 1);  --shifteaza bitul de start in bufferul de primire						
              rx_state <= receive;                                 
          ELSE                                                                                       
            rx_state <= idle;                                      
          END IF;
          
        WHEN receive =>                                       
          IF(rx_count < d_width) THEN                  --se compara numarul de biti primiti cu lungimea pachetului de biti                                      
            rx_count := rx_count + 1;                              
            rx_buffer <= rx & rx_buffer(d_width DOWNTO 1);  --shifteaza bitul nou primit in buffrul de primire
            rx_state <= receive;                                   
          ELSE                                                   
            rx_data <= rx_buffer(d_width DOWNTO 1);                --se trimite bufferul de primire
            rx_state <= idle;                                     
          END IF;
      END CASE;
    END IF;
  END PROCESS;
    
  --state machine pentru tx
  PROCESS(reset, clk)
    VARIABLE tx_count :  INTEGER RANGE 0 TO d_width+3 := 0;  --numara bitii transmisi
  BEGIN
    IF(reset = '0') THEN                                    
      tx_count := 0;                                            
      tx <= '1';                                               
      tx_state <= idle;                                         
    ELSIF(clk'EVENT AND clk = '1') THEN
      CASE tx_state IS
      
        WHEN idle =>                                              
          IF(tx_ena = '1') THEN                                     
            tx_buffer(d_width+1 DOWNTO 0) <=  tx_data & '0' & '1';    --se pun biti de start si de stop la finalul pachetului de date
            tx_count := 0;                                            
            tx_state <= transmit;                                    
          ELSE                                                      
            tx_state <= idle;                                         
          END IF;
          
        WHEN transmit =>                                          
          IF(baud_pulse = '1') THEN                                
            tx_count := tx_count + 1;                                
            tx_buffer <= '1' & tx_buffer(d_width+1 DOWNTO 1);  --se shifteaza un bit in bufferul de transmitere
          END IF;
          IF(tx_count < d_width+3) THEN                      
            tx_state <= transmit;                                    
          ELSE                                                      
            tx_state <= idle;                                         
          END IF;
      END CASE;
      tx <= tx_buffer(0);                                       --se transmite ultimul bit (de stop)
    END IF;
  END PROCESS;  
END logic;
