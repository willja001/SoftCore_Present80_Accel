-- progloader
-- PRESENT
-- Round Keys computed on the fly(using permutation)
-- Using Rotator
-- Set PMEM -> 8

LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

ENTITY progloader IS
	generic(
		LOADER_SIZE : integer:= 8
		);

	PORT(
		addr: IN STD_LOGIC_VECTOR(LOADER_SIZE - 1 DOWNTO 0);
		dout: OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
	);

END progloader;

ARCHITECTURE dataflow OF progloader IS

SIGNAL index: INTEGER RANGE 0 TO 2**LOADER_SIZE - 1;
TYPE vector_array IS ARRAY (0 to 2**LOADER_SIZE-1) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
CONSTANT memory : vector_array := 
	(

-- Assembled by SoftAsm.py at Sat Jan 14 09:37:12 2017
-- Start location: 0x000
-- End location: 0x08D
-- Highest program address: 0x09C
-- .equ x0 0x00
-- .equ x6 0x06
-- .equ x7 0x07
-- .equ cnt 0x08
-- .equ rnd 0x09
-- .equ rnd 0x09
-- .equ sbox 0x10
-- .equ k0 0x20
-- .equ k1 0x21
-- .equ k7 0x27
-- .lbl mnlp 0x000
-- .lbl sbst 0x002
-- .lbl sblp 0x009
-- .lbl perm 0x028
-- .lbl prm1 0x02C
-- .lbl prmnd 0x035
-- .lbl rtlp1 0x03A
-- .lbl rtnd1 0x043
-- .lbl rtlp2 0x05F
-- .lbl rtnd2 0x068
-- .lbl fnrndky 0x08B
-- .lbl lpnd 0x08D
-- .lbl rndky 0x08E
-- .lbl rndkylp 0x092
-- .lbl rndkynd 0x09C

x"90", --PC=0x000 jsr rndky
x"8E",
x"50", --PC=0x002 mvi cnt, r0
x"08",
x"51", --PC=0x004 mvi 0x07, r1
x"07",
x"24", --PC=0x006 sts r1, r0
x"52", --PC=0x007 mvi sbox, r2
x"10",
x"04", --PC=0x009 lds r1, r0
x"53", --PC=0x00A mvi 0x04, r3
x"04",
x"6C", --PC=0x00C srl r3, r0
x"5C",
x"6C", --PC=0x00E add r2, r0
x"08",
x"00", --PC=0x010 lds r0, r0
x"6C", --PC=0x011 sll r3, r0
x"7C",
x"05", --PC=0x013 lds r1, r1
x"6C", --PC=0x014 sll r3, r1
x"7D",
x"6C", --PC=0x016 srl r3, r1
x"5D",
x"6C", --PC=0x018 add r2, r1
x"09",
x"05", --PC=0x01A lds r1, r1
x"6C", --PC=0x01B lor r1, r0
x"34",
x"53", --PC=0x01D mvi cnt, r3
x"08",
x"0D", --PC=0x01F lds r3, r1
x"6C", --PC=0x020 prw r1, r0
x"B4",
x"65", --PC=0x022 dec r1
x"F4", --PC=0x023 bni perm
x"28",
x"27", --PC=0x025 sts r1, r3
x"D0", --PC=0x026 jmp sblp
x"09",
x"51", --PC=0x028 mvi 0x07, r1
x"07",
x"50", --PC=0x02A mvi x7, r0
x"07",
x"6C", --PC=0x02C prp r1, r2
x"D6",
x"28", --PC=0x02E sts r2, r0
x"64", --PC=0x02F dec r0
x"65", --PC=0x030 dec r1
x"F4", --PC=0x031 bni prmnd
x"35",
x"D0", --PC=0x033 jmp prm1
x"2C",
x"51", --PC=0x035 mvi k0, r1
x"20",
x"04", --PC=0x037 lds r1, r0
x"52", --PC=0x038 mvi 0x09, r2
x"09",
x"6C", --PC=0x03A prw r2, r0
x"B8",
x"66", --PC=0x03C dec r2
x"F4", --PC=0x03D bni rtnd1
x"43",
x"61", --PC=0x03F inc r1
x"04", --PC=0x040 lds r1, r0
x"D0", --PC=0x041 jmp rtlp1
x"3A",
x"50", --PC=0x043 mvi 0x09, r0
x"09",
x"6C", --PC=0x045 prs r0, r2
x"E2",
x"50", --PC=0x047 mvi sbox, r0
x"10",
x"49", --PC=0x049 mov r2, r1
x"53", --PC=0x04A mvi 0x04, r3
x"04",
x"6C", --PC=0x04C srl r3, r2
x"5E",
x"6C", --PC=0x04E add r2, r0
x"08",
x"00", --PC=0x050 lds r0, r0
x"6C", --PC=0x051 sll r3, r0
x"7C",
x"6C", --PC=0x053 sll r3, r1
x"7D",
x"6C", --PC=0x055 srl r3, r1
x"5D",
x"6C", --PC=0x057 lor r1, r0
x"34",
x"51", --PC=0x059 mvi k0, r1
x"20",
x"21", --PC=0x05B sts r0, r1
x"61", --PC=0x05C inc r1
x"52", --PC=0x05D mvi 0x08, r2
x"08",
x"6C", --PC=0x05F prs r2, r0
x"E8",
x"21", --PC=0x061 sts r0, r1
x"66", --PC=0x062 dec r2
x"F4", --PC=0x063 bni rtnd2
x"68",
x"61", --PC=0x065 inc r1
x"D0", --PC=0x066 jmp rtlp2
x"5F",
x"50", --PC=0x068 mvi rnd, r0
x"09",
x"00", --PC=0x06A lds r0, r0
x"60", --PC=0x06B inc r0
x"41", --PC=0x06C mov r0, r1
x"52", --PC=0x06D mvi 0x01, r2
x"01",
x"6C", --PC=0x06F srl r2, r0
x"58",
x"52", --PC=0x071 mvi k7, r2
x"27",
x"0B", --PC=0x073 lds r2, r3
x"B3", --PC=0x074 xor r0, r3
x"2E", --PC=0x075 sts r3, r2
x"62", --PC=0x076 inc r2
x"50", --PC=0x077 mvi 0x07, r0
x"07",
x"6C", --PC=0x079 sll r0, r1
x"71",
x"0B", --PC=0x07B lds r2, r3
x"B7", --PC=0x07C xor r1, r3
x"2E", --PC=0x07D sts r3, r2
x"51", --PC=0x07E mvi rnd, r1
x"09",
x"04", --PC=0x080 lds r1, r0
x"52", --PC=0x081 mvi 0x1E, r2
x"1E",
x"6C", --PC=0x083 sub r0, r2
x"12",
x"F8", --PC=0x085 bzi fnrndky
x"8B",
x"60", --PC=0x087 inc r0
x"21", --PC=0x088 sts r0, r1
x"D0", --PC=0x089 jmp mnlp
x"00",
x"90", --PC=0x08B jsr rndky
x"8E",
 -------PC=0x08D---- end -----
x"40", --PC=0x08D nop
x"50", --PC=0x08E mvi k0, r0
x"20",
x"51", --PC=0x090 mvi x7, r1
x"07",
x"06", --PC=0x092 lds r1, r2
x"03", --PC=0x093 lds r0, r3
x"BE", --PC=0x094 xor r3, r2
x"29", --PC=0x095 sts r2, r1 
x"65", --PC=0x096 dec r1
x"F4", --PC=0x097 bni rndkynd
x"9C",
x"60", --PC=0x099 inc r0
x"D0", --PC=0x09A jmp rndkylp
x"92",
x"A0", --PC=0x09C ret
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00",
x"00"


);

BEGIN

	index <= to_integer(unsigned(addr));
	dout <= memory(index);

END dataflow;
