
-------------------------------------------------------------------------------
--! @file       Datapath.vhd
--! @author     William Diehl
--! @brief      
--! @date       4 Dec 2016
-------------------------------------------------------------------------------

-- SoftCore Datapath

library ieee;
use ieee.std_logic_1164.all; 
use ieee.std_logic_unsigned.all;  
use IEEE.NUMERIC_STD.ALL;

entity Datapath is

	generic (
		G_DMEM_SIZE : integer:= 16;
		G_PMEM_SIZE : integer:= 12
		);

	port (

		clk : in std_logic;

		-- ext data signals

		extdaddr	 	: in std_logic_vector(15 downto 0);
		extdout		: out std_logic_vector(7 downto 0);
		extdin		: in std_logic_vector(7 downto 0);
		extprogin	: in std_logic_vector(7 downto 0);
		extPC		: in std_logic_vector(11 downto 0);
		
		startloc	: in std_logic_vector(11 downto 0);
		endloc		: in std_logic_vector(11 downto 0);

		-- ext control signals

		extprogwrite	: in std_logic;
		extmemwrite	: in std_logic;
		extwrite : in std_logic;
		done		: out std_logic;

		-- int control signals

		PCen		: in std_logic;
		SPen		: in std_logic;
		SRen		: in std_logic;

		regwrite	: in std_logic;
		memwrite	: in std_logic;
		dstsel		: in std_logic;
		dregsel		: in std_logic_vector(2 downto 0);
		dextregsel	: in std_logic_vector(2 downto 0);
		rxsel		: in std_logic_vector(2 downto 0);
		PCsel		: in std_logic_vector(1 downto 0);
		dinsel		: in std_logic_vector(2 downto 0);
		SPsel		: in std_logic_vector(1 downto 0);
		SRsel		: in std_logic;
		SRsrcsel	: in std_logic_vector(1 downto 0);
		SPaddsel	: in std_logic;
		PCabssel	: in std_logic_vector(1 downto 0);
		PChighregsel	: in std_logic_vector(1 downto 0);
		progword	: out std_logic_vector(7 downto 0);
		SR_out	: out std_logic_vector(7 downto 0)
		);

end Datapath;

architecture structural of Datapath is
 
constant SPinit : std_logic_vector(15 downto 0):= (OTHERS => '1'); --  initial stack pointer

signal PChighregen: std_logic;

signal dstreg : std_logic_vector(1 downto 0);
signal rxnext : std_logic_vector(7 downto 0);
signal r0, r1, r2, r3 : std_logic_vector(7 downto 0);
signal PC, PCnext, PCabs, PCbxx : std_logic_vector(11 downto 0);
signal paddr : std_logic_vector(15 downto 0);
signal SP : std_logic_vector(15 downto 0);

signal r0en, r1en, r2en, r3en : std_logic;
signal r0dec, r1dec, r2dec, r3dec : std_logic;
signal ALUop1sel, ALUop2sel : std_logic_vector(1 downto 0);
signal ALU2opsel : std_logic_vector(3 downto 0);
signal ALUop1, ALUop2, ALU1out, ALU2out: std_logic_vector(7 downto 0);
signal ALU2add, ALU2sub, ALU2and, ALU2lor, ALU2xor, ALU2ror, ALU2rol : std_logic_vector(7 downto 0);
signal ALU2gf2, ALU2gf3, ALU2gf4 : std_logic_vector(7 downto 0);
signal ALU2gf4_lsn : std_logic_vector(3 downto 0);
signal ALU2add9, ALU1inc9 : std_logic_vector(8 downto 0);
signal ALU1inc, ALU1dec, ALU1not : std_logic_vector(7 downto 0);
signal ALU2addc, ALU1incc : std_logic;
signal SR_ALUin, SRnext : std_logic_vector(7 downto 0);
signal C_ALUin, Z_ALUin, N_ALUin, Cnext, Znext, Nnext : std_logic;
signal dstdecin : std_logic_vector(1 downto 0);
signal prog_out, next_prog_out, dout : std_logic_vector(7 downto 0);
signal TRF0out, TRF1out, TRF2out, TRF3out, TRFout : std_logic_vector(7 downto 0);
signal TRFsel : std_logic_vector(1 downto 0);
signal SPin, SPpX : std_logic_vector(15 downto 0);
signal din, ddin : std_logic_vector(7 downto 0);
signal we : std_logic;
signal daddr, ddaddr : std_logic_vector(15 downto 0);
signal ddaddrl, ddaddru : std_logic_vector(7 downto 0);
signal PChighregin, PChighreg : std_logic_vector(3 downto 0);
signal PCabsl : std_logic_vector(7 downto 0);
signal SR : std_logic_vector(7 downto 0);

-- optional permutator signals

signal p0en, p1en, p2en, p3en, p4en, p5en, p6en, p7en : std_logic;
signal p0dec, p1dec, p2dec, p3dec, p4dec, p5dec, p6dec, p7dec : std_logic;
signal pregwrite : std_logic;
signal pregsel : std_logic_vector(3 downto 0);
signal permsel : std_logic_vector(2 downto 0);
signal ALUperm : std_logic_vector(7 downto 0);
signal p0, p1, p2, p3, p4, p5, p6, p7 : std_logic_vector(7 downto 0);
signal perm0, perm1, perm2, perm3, perm4, perm5, perm6, perm7 : std_logic_vector(7 downto 0); 

-- end permutator signals

-- optional permutator + rotator signals
-- Note: the optional rotator only works with the above optional permutator

signal p8en, p9en: std_logic;
signal p8dec, p9dec: std_logic;
signal p8, p9 : std_logic_vector(7 downto 0);
signal prot61_0, prot61_1, prot61_2, prot61_3, prot61_4, prot61_5, prot61_6, prot61_7,
       prot61_8, prot61_9 : std_logic_vector(7 downto 0);  
signal ALUrot61 : std_logic_vector(7 downto 0);
signal pword, pwordrot61 : std_logic_vector(79 downto 0);
       
-- end optional rotator signals
 
begin

-- general use registers

with rxsel select
    	rxnext <= ALUop1 when "000",			
		  dout   when "001",			
	     prog_out when "010",			
	     ALU1out when "011",
		  ALU2out when "100",
		  TRFout when "101",
		  ALU2xor when "110",
	     ALUop1 when others;

r0en <= regwrite and r0dec;

store_r0: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => rxnext,
		clk => clk,
		en => r0en,
		q => r0);

r1en <= regwrite and r1dec;

store_r1: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => rxnext,
		clk => clk,
		en => r1en,
		q => r1);

r2en <= regwrite and r2dec;

store_r2: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => rxnext,
		clk => clk,
		en => r2en,
		q => r2);

r3en <= regwrite and r3dec;

store_r3: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => rxnext,
		clk => clk,
		en => r3en,
		q => r3);

-- end registers

-- ALU common selectors

-- src

ALUop1sel <= prog_out(3 downto 2);

with ALUop1sel select
	ALUop1 <= r0 when "00",
		   r1 when "01",
		   r2 when "10",
			r3 when "11",
		   r3 when others;

-- dst

ALUop2sel <= prog_out(1 downto 0);

with ALUop2sel select
	ALUop2 <= r0 when "00",
		  r1 when "01",
		  r2 when "10",
		  r3 when "11",
		  r3 when others;

-- ALU 1

ALU1inc9 <= ('0' & ALUop2) + 1;
ALU1inc <= ALU1inc9(7 downto 0);
ALU1incc <= ALU1inc9(8);

ALU1dec <= ALUop2 - 1;
ALU1not <= not ALUop2;

with ALUop1sel select
	ALU1out <= ALU1inc when "00",
		        ALU1dec when "01",
--		        ALU1not when "10",
		        ALUop2 when others;

-- ALU2

ALU2add9 <= ('0' & ALUop1) + ('0' & ALUop2);
ALU2add <= ALU2add9(7 downto 0);
ALU2addc <= ALU2add9(8);

ALU2sub <= ALUop1 - ALUop2;
--ALU2and <= ALUop1 and ALUop2;
ALU2lor <= ALUop1 or ALUop2;
ALU2xor <= ALUop1 xor ALUop2;

ALU2opsel <= prog_out(7 downto 4);

-- 0000 = ADD
-- 0001 = SUB
-- 0010 = AND
-- 0011 = (L)OR
-- 0100 = ROR
-- 0101 = SLR
-- 0110 = ROL
-- 0111 = SLL
--------------- optional additional functions
-- 1000 = GF4 (GF(2^4) mod x^4 + x + 1)
-- 1001 = GF2 (GF(2^8) mod x^8 + x^4 + x^3 + x + 1)
-- 1010 = GF3 (GF(2^8) mod x^8 + x^4 + x^3 + x + 1)
-- 1011 = PRW (write to permutator)
-- 1100 = PRR (read from permutator registers - no permutation)
-- 1101 = PRP (read from permutator registers - permutation)
-- 1110 = PRS (read from permutator registers - left rotation by 61 bits 

rot_right: entity work.rotate_right(structural)
	port map(
		input => ALUop2,
    	distance => ALUop1(2 downto 0),
		shift => ALU2opsel(0),
		result => ALU2ror
		);

rot_left: entity work.rotate_left(structural)
	port map(
		input => ALUop2,
		distance => ALUop1(2 downto 0),
		shift => ALU2opsel(0),
		result => ALU2rol
		);

-- optional ALU2 instructions for AES GF2 and GF3 multiplication
--gfmul : entity work.GF_mul(dataflow)
--	port map(
--		input => ALUop1,
--		gf2out => ALU2gf2,
--		gf3out => ALU2gf3
--		);
		
-- optional ALU2 instructions for LED GF4 (GF2^4) multiplication
--gf4mul : entity work.GF4multiply(structural)
--	port map(
--		a => ALUop1(3 downto 0),
--		b => ALUop2(3 downto 0),
--		y => ALU2gf4_lsn
--		);

--ALU2gf4 <= x"0" & ALU2gf4_lsn;

with ALU2opsel select
	ALU2out <= ALU2add when "0000",
		       ALU2sub when "0001",
--		       ALU2and when "0010",
		       ALU2lor when "0011",
--		       ALU2ror when "0100",
       		   ALU2ror when "0101",
--		       ALU2rol when "0110",
		       ALU2rol when "0111",
--	           ALU2gf4 when "1000",
--			   ALU2gf2 when "1001",
--			   ALU2gf3 when "1010",
			   ALUop2 when "1011",
			   ALUperm when "1101",
			   ALUrot61 when "1110",
		       ALUop2 when others;

-- end ALU

-- status register
-- -|-|-|-|-|C|N|Z
-- C = set on carry
-- Z = set on zero
-- N = set on negative

SR_ALUin <= ALU1out when (SRsel ='1') else ALU2out;

C_ALUin  <= ALU1incc when (SRsel = '1') else ALU2addc;
N_ALUin <= '1' when (SR_ALUin(7) = '1') else '0';
Z_ALUin <= '1' when (SR_ALUin = 0) else '0';

with SRsrcsel select
	     Cnext <= '0' when "00",
		  C_ALUin when "01",
		  dout(2) when "10",
		  '0' when others;

with SRsrcsel select
	Nnext <= '0' when "00",
		  N_ALUin when "01",
		  dout(1) when "10",
		  '0' when others;

with SRsrcsel select
	Znext <= '0' when "00",
		  Z_ALUin when "01",
		  dout(0) when "10",
		  '0' when others;

SRnext <= "00000" & Cnext & Nnext & Znext;

store_SR: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => SRnext,
		clk => clk,
		en => SRen,
		q => SR);
  
SR_out <= SR;

-- end status register

-- destination decoder

dstdecin <= prog_out(1 downto 0) when (dstsel = '0') else dstreg;

store_dstreg: entity work.regn(behavioral)
	generic map(n=>2)
	port map(
		d => prog_out(1 downto 0),
		clk => clk,
		en => '1',
		q => dstreg);

r0dec <= '1' when (dstdecin = "00") else '0';
r1dec <= '1' when (dstdecin = "01") else '0';
r2dec <= '1' when (dstdecin = "10") else '0';
r3dec <= '1' when (dstdecin = "11") else '0';

-- end destination decoder

-- Transformation Function 

--TRF0: entity work.TRF_PRESENT(dataflow) --TRF0(dataflow)
--	generic map (TRF_SIZE => 8)
--	port map(
--		din => ALUop2,
--		dout => TRF0out
--		);

--TRF1: entity work.TRF_AES_1(dataflow)
--	generic map (TRF_SIZE => 8)
--	port map(
--		din => ALUop2,
--		dout => TRF1out
--		);

--TRF2: entity work.TRF_AES(dataflow) --TRF2(dataflow)
--	generic map (TRF_SIZE => 8)
--	port map(
--		din => ALUop2,
--		dout => TRF2out
--		);

--TRF3: entity work.TRF_AES(dataflow) --TRF3(dataflow)
--	generic map (TRF_SIZE => 8)
--	port map(
--		din => ALUop2,
--		dout => TRF3out
--		);

TRFsel <= prog_out(3 downto 2); 

--with TRFsel select
--	TRFout <= TRF0out when "00",
--		  TRF1out when "01",
		  --TRF2out when "10",
		  --TRF3out when "11",
--		  TRF0out when others;

-- stack pointer

with SPsel select
	SPin <= SPinit when "00",
		SPinit when "01",
		(SP + 1) when "10",
	   (SP - 1) when "11",
		SPinit when others;

store_SP: entity work.regn(behavioral)
	generic map(n=>16)
	port map(
		d => SPin,
		clk => clk,
		en => SPen,
		q => SP);

SPpX <= (SP + 1) when (SPaddsel = '1') else (SP + 2);

-- data memory

with dinsel select
	ddin <= r0 when "000",
           r1 when "001",
           r2 when "010",
			  r3 when "011",
           SR when "100",
	        (PC(7 downto 0) + 1) when "101",
			  (x"0" & PC(11 downto 8)) when "111",
		     x"00" when others;

din <= ddin when (extwrite = '0') else extdin; -- external memory write
we <= memwrite when (extwrite = '0') else extmemwrite;
daddr <= ddaddr when (extwrite = '0') else extdaddr;

with dregsel select
	ddaddrl <= r0 when "000",
                 r1 when "001",
                 r2 when "010",
                 r3 when "011",
		 SP(7 downto 0) when "100",
		 SPpX(7 downto 0) when "110",
		 x"00" when others;

with dextregsel select
	ddaddru <= r1 when "000",
              r3 when "001",
              x"00" when "010",
              x"00" when "011",
		 SP(15 downto 8) when "100",
		 SPpX(15 downto 8) when "110",
		 x"00" when others;

ddaddr <= ddaddru & ddaddrl;
 
--! Note: Addressable data RAM is 2^G_DMEM_SIZE bytes
--! Default of G_DMEM_SIZE is 16 = 2^16 = 64K RAM

dataMemory: entity work.DRAM(behavioral)
	generic map( MEM_SIZE => G_DMEM_SIZE)
	port map(
			clk => clk,
			we => we,
	      di => din,
			do => dout,
			addr => daddr);

extdout <= dout;

-- program counter and control logic

with PChighregsel select
	PChighregin <= prog_out(3 downto 0) when "00",
		       prog_out(3 downto 0) when "01",
		       dout(3 downto 0) when "10", -- RET
		       PC(11 downto 10) & prog_out(1 downto 0) when "11", -- BXI
		       prog_out(3 downto 0) when others;

store_PChighreg: entity work.regn(behavioral)
	generic map(n=>4)
	port map(
		d => PChighregin,
		clk => clk,
		en => '1',
		q => PChighreg);

with PCabssel select
	PCabsl <= ALUop2 when "00", -- r3, r2, r1, r0 in dst field - BXX
		  ALUop2 when "01", -- r3, r2, r1, r0 in dst field - BXX
 		  dout when "10", -- RET
		  prog_out when "11", -- BXI, JSR, JMP
		  prog_out when others;

PCabs <= PChighreg & PCabsl;
PCbxx <= PC(11 downto 8) & PCabsl;

with PCsel select
	PCnext <= startloc when "00",
             PCbxx when "01",
				 (PC + 1) when "10",
             PCabs when "11",
				 (PC + 1) when others;

store_PC: entity work.regn(behavioral)
	generic map(n=>12)
	port map(
		d => PCnext,
		clk => clk,
		en => PCen,
		q => PC);

done <= '1' when (PC = endloc) else '0';

-- paddr uses PCnext due to pipelining of program word from memory
paddr <= (x"0" & PCnext) when (extprogwrite = '0') else (x"0" & extPC);

--! Max Program memory is 2^12  

progMemory: entity work.DRAM(behavioral)
	generic map( MEM_SIZE => G_PMEM_SIZE)
	port map(
		clk => clk,
		we => extprogwrite, -- program memory cannot be written inside softcore
	   di => extprogin, 
		do => next_prog_out,  
		addr => paddr);

--! The prog_out register is pipelined to reduce critical path

store_prog_out: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => next_prog_out,
		clk => clk,
		en => PCen,
		q => prog_out);

progword <= prog_out;

--! Optional 64-bit Permutator and Rotator used for PRESENT
--! Comment this section out if not using the permutator

-- 64-bit permutation matrix

perm7 <= p7(7) & p7(3) & p6(7) & p6(3) & p5(7) & p5(3) & p4(7) & p4(3);
perm6 <= p3(7) & p3(3) & p2(7) & p2(3) & p1(7) & p1(3) & p0(7) & p0(3);

perm5 <= p7(6) & p7(2) & p6(6) & p6(2) & p5(6) & p5(2) & p4(6) & p4(2);
perm4 <= p3(6) & p3(2) & p2(6) & p2(2) & p1(6) & p1(2) & p0(6) & p0(2);

perm3 <= p7(5) & p7(1) & p6(5) & p6(1) & p5(5) & p5(1) & p4(5) & p4(1);
perm2 <= p3(5) & p3(1) & p2(5) & p2(1) & p1(5) & p1(1) & p0(5) & p0(1);

perm1 <= p7(4) & p7(0) & p6(4) & p6(0) & p5(4) & p5(0) & p4(4) & p4(0);
perm0 <= p3(4) & p3(0) & p2(4) & p2(0) & p1(4) & p1(0) & p0(4) & p0(0);

-- left 61-bit rotation

pword <= p9 & p8 & p7 & p6 & p5 & p4 & p3 & p2 & p1 & p0;
pwordrot61 <= pword(18 downto 0) & pword(79 downto 19);
prot61_9 <= pwordrot61(79 downto 72);
prot61_8 <= pwordrot61(71 downto 64);
prot61_7 <= pwordrot61(63 downto 56);
prot61_6 <= pwordrot61(55 downto 48);
prot61_5 <= pwordrot61(47 downto 40);
prot61_4 <= pwordrot61(39 downto 32);
prot61_3 <= pwordrot61(31 downto 24);
prot61_2 <= pwordrot61(23 downto 16);
prot61_1 <= pwordrot61(15 downto 8);
prot61_0 <= pwordrot61(7 downto 0);

-- expanded to width of 4 bits for rotator
-- reduce to 2 downto 0 for permutator alone
pregsel <= ALUop1(3 downto 0);
permsel <= pregsel(2 downto 0);

with permsel select
	         ALUperm <= perm0 when "000",
	                    perm1 when "001",
	                    perm2 when "010",
	                    perm3 when "011",
                        perm4 when "100",
                        perm5 when "101",
                        perm6 when "110",
                        perm7 when "111",
                        perm0 when others;
 
-- optional rotator selector

with pregsel select
	         ALUrot61 <= prot61_0 when "0000",
	                     prot61_1 when "0001",
                         prot61_2 when "0010",
                         prot61_3 when "0011",
                         prot61_4 when "0100",
                         prot61_5 when "0101",
                         prot61_6 when "0110",
                         prot61_7 when "0111",
                         prot61_8 when "1000",
                         prot61_9 when "1001",
                         prot61_0 when others;
                      
                         
pregwrite <= '1' when (regwrite = '1' and ALU2opsel = "1011") else '0';

-- preg decoder

--  pregsel expanded to width of 4 bits for rotator
-- reduce to width of 3 bits for permutator alone

p0dec <= '1' when (pregsel = "0000") else '0';
p1dec <= '1' when (pregsel = "0001") else '0';
p2dec <= '1' when (pregsel = "0010") else '0';
p3dec <= '1' when (pregsel = "0011") else '0';
p4dec <= '1' when (pregsel = "0100") else '0';
p5dec <= '1' when (pregsel = "0101") else '0';
p6dec <= '1' when (pregsel = "0110") else '0';
p7dec <= '1' when (pregsel = "0111") else '0';

-- expanded for rotator

p8dec <= '1' when (pregsel = "1000") else '0';
p9dec <= '1' when (pregsel = "1001") else '0';

-- permutator register bank

store_p0: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p0en,
		q => p0);

p0en <= pregwrite and p0dec;

store_p1: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p1en,
		q => p1);

p1en <= pregwrite and p1dec;

store_p2: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p2en,
		q => p2);

p2en <= pregwrite and p2dec;

store_p3: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p3en,
		q => p3);

p3en <= pregwrite and p3dec;

store_p4: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p4en,
		q => p4);

p4en <= pregwrite and p4dec;

store_p5: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p5en,
		q => p5);

p5en <= pregwrite and p5dec;

store_p6: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p6en,
		q => p6);

p6en <= pregwrite and p6dec;

store_p7: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p7en,
		q => p7);

p7en <= pregwrite and p7dec;

-- end permutator

-- optional rotator registers

store_p8: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p8en,
		q => p8);

p8en <= pregwrite and p8dec;

store_p9: entity work.regn(behavioral)
	generic map(n=>8)
	port map(
		d => ALUop2,
		clk => clk,
		en => p9en,
		q => p9);

p9en <= pregwrite and p9dec;

-- end optional rotator registers

end structural; 