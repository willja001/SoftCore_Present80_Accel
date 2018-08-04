-- dataloader
-- PRESENT (with permutation - round keys computed on the fly)
-- Using rotator
-- Set DMEM Generic to 6

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

ENTITY dataloader IS
	generic(
		LOADER_SIZE : integer:= 8
		);

	PORT(
		addr: IN STD_LOGIC_VECTOR(LOADER_SIZE - 1 DOWNTO 0);
		dout: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);

END dataloader;

ARCHITECTURE dataflow OF dataloader IS

SIGNAL index: INTEGER RANGE 0 TO 2**LOADER_SIZE - 1;
TYPE vector_array IS ARRAY (0 to 2**LOADER_SIZE-1) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
CONSTANT memory : vector_array := 
	(

-- 00 - 07 status x0 - x7 
x"EF",x"CD",x"AB",x"89",x"67",x"45",x"23",x"01",
-- 08 - 0F variables
x"00",x"00",x"00",x"00",x"00",x"00",x"00",x"00",
-- 10 - 1F S Box table
x"0C",x"05",x"06",x"0B",x"09",x"00",x"0A",x"0D",
x"03",x"0E",x"0F",x"08",x"04",x"07",x"01",x"02",

-- key data
x"FF", x"FF", x"FF", x"FF", x"FF", x"FF", x"FF", x"FF",
x"FF", x"FF", x"00", x"00", x"00", x"00", x"00", x"00",
-- not used
x"00", x"00", x"00", x"00", x"00", x"00", x"00", x"00",
x"00", x"00", x"00", x"00", x"00", x"00", x"00", x"00"

);
BEGIN

	index <= to_integer(unsigned(addr));
	dout <= memory(index);

END dataflow;
