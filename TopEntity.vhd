----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:12:12 10/24/2014 
-- Design Name: 
-- Module Name:    SevenSeg - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.MATH_REAL.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD;
use IEEE.NUMERIC_STD.ALL;
use DigitalClock.types;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity SevenSeg is
  port ( Seg7    : OUT STD_LOGIC_VECTOR(6 downto 0)
       ; Seg7_AN : OUT STD_LOGIC_VECTOR(3 downto 0)
       ; Seg7_DP : OUT STD_LOGIC
       ; Switch  : IN  STD_LOGIC_VECTOR(7 downto 0)
       ; Led     : OUT STD_LOGIC_VECTOR(7 downto 0)
       ; clk     : IN  STD_LOGIC
       );
end SevenSeg;

function To_Std_Logic(L: BOOLEAN) return std_ulogic is
begin
  if L then
    return('1');
  else
    return('0');
  end if;
end function To_Std_Logic;

function From_Std_Logic(L: STD_LOGIC) return BOOLEAN is
begin
  case L of
    '0' => return True;
    '1' => return False;
  end case;
end function From_Std_Logic;

ARCHITECTURE Behavioral OF SevenSeg IS
  component topEntity_0 is
  port(clk            : in boolean;
       system1000     : in std_logic;
       system1000_rst : in std_logic;
       topLet_o       : out product6);
  end;
BEGIN
  t : topEntity_0 PORT MAP (
      clk_i1         => From_Std_Logic(clk)
    , system1000     => clk
    , system1000_rst => OPEN
    , topLet_o       => recordish
  )
  Seg7_an <= recordish.product6_sel0;
  Seg7    <= recordish.product6_sel1;
  Seg7_DP <= '1';
  Led     <= Switch;
END Behavioral;

