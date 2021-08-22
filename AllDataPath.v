/*
 *  File: 		AllDataPath.v
 *
 *  Description:
 *				Combination of all modules in the files SimpleDataPathWithControlAndALUControl.v and RegistersDataPath.v in addition to
 *				extra modules that allow for the instructions listed below:
 *								- add			- sra			- jal
 *								- and			- srl			- jr
 *								- div			- sub			- lw
 *								- mul			- lui			- sw
 *								- or			- beq			- mfhi
 *								- sll			- bne			- mflo
 *								- slt			- j				- syscall
 *
 *	Modules:
 *				testBench		- a module for testing whether the program functions by connecting all of the modules
 *				DP				- a module that contains datapath elements
 *				C				- a module that contains control elements
 *				Clock			- a module that changes its output signal every tick
 *				PC				- a module that maintains the current address and adjusts it to the next address during positive clock edges
 *				IM				- a module that reads an instruction from memory based on the current address
 *				DM				- a module that reads and writes to memory using separate controls for reading and writing (mutually exclusive) and where writing only occurs during positive clock edges
 *				RF				- a module that will always output the contents of the registers corresponding to the inputs and that will only write during positive clock edges, if provided a signal
 *				HILO			- a module that stores the HI and LO registers that are utilized by the ALU
 *				ALU				- a module that performs different operations dependent on its ALUConOut signal and outputs the zero or nonzero result produced by the operation
 * 				Control			- a module that outputs the appropriate flags based on the op codes inputted to it
 *				ALUControl		- a module that outputs a unique output for each command depending on the ALUOp and (possibly) functCode inputted into it
 *				Adder			- a module which calculates the sum of an increment and an addend
 *				SignExtend		- a module that extends a 16 bit address to produce a 32 bit address by using the 16th bit of the 16 bit address
 *				ShiftLeft2		- a module that shifts its input to the left twice by concatenating it with two zeros at the end
 *				Mux				- a module that selects one of two inputs inputted to it depending on the selector bit also inputted to it
 */

 
/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	testBench			- a module for testing whether the program functions by connecting all of the modules
 *	Ports:	None
 */
module testBench;											// establish the testBench module
	wire regDst, jump, jal, beq, bne, memRead, memToReg,	// establish regDst, jump, jal, beq, bne, memRead, memToReg,
		memWrite, ALUSrc, regWrite, shift, jr, zero;		//		memWrite, ALUSrc, regWrite, shift, jr, and zero as 1 bit wires
	wire [4:0] ALUConOut;									// establish ALUConOut as a 5 bit wire
	wire [31:0] instruction;
	
	DP dp(regDst, jump, jal, beq, bne, memRead,	memToReg,	// instantiate a DP for datapath elements
		memWrite, ALUSrc, regWrite, shift, jr, ALUConOut,
		instruction);
	C c(instruction, regDst, jump, jal, beq, bne, memRead,	// instantiate a C for control elements
		memToReg, memWrite, ALUSrc, regWrite, ALUConOut,
		shift, jr);
	
	reg exitLoop;											// establish exitLoop as a 1 bit reg
	reg [4:0] i, j;											// establish i and j as 5 bit regs
	reg [7:0] char;											// establish char as an 8 bit reg
	
	always @ (ALUConOut)									// every time that the ALUConOut changes:
	begin
		if (ALUConOut == 6'b10111) 							// 		if the instruction is a syscall:
		begin
			case (dp.rf.mem[993])
				1: $write ("%d", dp.rf.mem[995]);			//			if register $v0 contains a 1, then the integer value in register $a0 is outputted
				4:											//			if register $v0 contains a 4, then the data beginning at the address in register $a0 is outputted as a string
				begin exitLoop = 0;							//				string is outputted one character at a time until a NULL character is found by reading the data eight bits at a time:
					for(j = 0; exitLoop == 0; j = j + 1)						// outer loop for looping through lines in data memory
					begin
						for(i = 0; i < 24 && exitLoop == 0; i = i + 8)			// inner loop for looping through eight bits (from line in outer loop) in data memory
						begin
							char = dp.dm.mem[dp.rf.mem[995][10:2]+j][i+:7];		// set char to be the next character of line
							if(char == 0)										// if next character is the NULL character, exit the loop
								exitLoop = 1;
							$write("%s", char);									// print the next character
						end
						if(exitLoop == 0)
						begin
							char = dp.dm.mem[dp.rf.mem[995][10:2]+j][31:24];	// set char to be the last character of line (this must be done separately, otherwise causes an infinite loop)
							if(char == 0)										// if last character is the NULL character, exit the loop
								exitLoop = 1;
							$write("%s", char);									// print last character
						end
					end
				end
				10:	$finish;								//			if register $v0 contains a 10, then the program is terminated
			endcase
		end
	end
	
	initial													// initially:
	begin
		$readmemb("textSegment.txt", dp.im.mem);			//		set up the memory of IM
		$readmemb("dataSegment.dat", dp.dm.mem);			//		set up the memory of DM
		$readmemb("dataSegment.dat", dp.rf.mem);			//		set up the memory of Registers
	
		dp.c.clock = 0;										// 		initial values
		dp.pc.currentAddress = 32'h3000;
		dp.im.instruction =
			dp.im.mem[dp.currentAddress[7:2]];
		dp.hl.HI = 0; dp.alu.newHI = 0;
		dp.hl.LO = 0; dp.alu.newLO = 0;
		
		/* TEST ALL MODULES
		 *	Testing Clock Module
		 *		$monitor("time=%t: clock=%d", $time, dp.clock);
		 *	Testing PC Module
		 *		$monitor("time=%t: clock=%d, address=%h", $time, dp.clock, dp.currentAddress);
		 *	Testing IM Module
		 *		$monitor("time=%t: clock=%d, address=%h, instruction=%h", $time, dp.clock, dp.currentAddress, instruction);
		 *	Testing DM Module
		 *		wire [31:0] contentsOfAddress;
		 *		assign contentsOfAddress = dp.dm.mem[dp.ALUResult[10:2]];
		 *		$monitor("time=%t: clock=%d, address=%h, writeData=%d, memWrite=%d, memRead=%d, readData=%d, contents of address=%d", $time, dp.clock, dp.ALUResult, dp.data2, memWrite, memRead, dp.readData, contentsOfAddress);
		 *	Testing RF Module
		 *		wire [31:0] contentsOfWriteReg;
		 *		assign contentsOfWriteReg = dp.rf.mem[dp.writeReg+991];
		 *		$monitor("time=%t: clock=%d, readReg1=%d, readReg2=%d, writeReg=%d, writeData=%d, regWrite=%d, data1=%d, data2=%d, contents of writeReg=%d", $time, dp.clock, instruction[25:21], instruction[20:16], dp.writeReg, dp.writeData, regWrite, dp.data1, dp.data2, contentsOfWriteReg);
		 *	Testing HILO Module
		 *		$monitor("time=%t: clock=%d, newHI=%d, newLO=%d, HI=%d, LO=%d", $time, dp.clock, dp.newHI, dp.newLO, dp.HI, dp.LO);
		 *		#1 dp.alu.newHI = 1;
		 *		#1 dp.alu.newLO = 2;
		 *		#1 dp.alu.newLO = 3;
		 *	Testing ALU Module
		 *		$monitor("time=%t: ALUIn1=%d, ALUIn2=%d, ALUConOut=%d, ALUResult=%d, zero=%d", $time, dp.ALUIn1, dp.ALUIn2, ALUConOut, dp.ALUResult, dp.zero);
		 *	Testing Control Module
		 *		$monitor("time=%t: op code=%d, jump=%d, jal=%d, beq=%d, bne=%d, memRead=%d, memToReg=%d, ALUOp=%b, memWrite=%d, ALUSrc=%d, regWrite=%d", $time, instruction[31:26], regDst, jump, jal, beq, bne, memRead, memToReg, c.ALUOp, memWrite, ALUSrc, regWrite);
		 *	Testing ALUControl Module
		 *		$monitor("time=%t: ALUOp=%b, funct code=%h, ALUConOut=%b, shift=%d, jr=%d", $time, c.ALUOp, instruction[5:0], ALUConOut, shift, jr);
		 *	Testing Adder Module
		 *		$monitor("time=%t: currentAddress=%h, newAddress=%h", $time, dp.currentAddress, dp.newAddress);
		 *	Testing SignExtend Module
		 *		$monitor("time=%t: instruction[15:0]=%h, extendedAddress=%h", $time, instruction[15:0], dp.extendedAddress);
		 *	Testing ShiftLeft2 Module
		 *		$monitor("time=%t: instruction[25:0]=%b, slOut=%b", $time, instruction[25:0], dp.slOut);
		 *	Testing Mux Module
		 *		$monitor("time=%t: memToReg/sel=%d, ALUResult/in1=%d, readData/in2=%d, out=%d", $time, memToReg, dp.ALUResult, dp.readData, dp.m3Out);
		 */
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	DP					- a module that contains datapath elements
 *	Ports:
 *			regDst				- a 1 bit input wire that determines which register to use as the write register
 *			jump				- a 1 bit input wire that determines whether a jump is needed
 *			jal					- a 1 bit input wire that determines whether a jal is occurring
 *			beq					- a 1 bit input wire that determines whether a beq is occurring
 *			bne					- a 1 bit input wire that determines whether a bne is occurring
 *			memRead				- a 1 bit input wire that determines whether the memory needs to be read from
 *			memToReg			- a 1 bit input wire that determines whether the information read from memory needs to be written to a register
 *			memWrite			- a 1 bit input wire that determines whether the memory needs to be written to
 *			ALUSrc				- a 1 bit input wire that determines the second input to the ALU
 *			regWrite			- a 1 bit input wire that determines whether the registers need to be written to
 *			shift				- a 1 bit input wire flag indicating whether a shift is occurring
 *			jr					- a 1 bit input wire flag indicating whether a jr is occurring
 *			ALUConOut			- a 5 bit input wire representing the identification of the command being read from instruction
 *			instruction			- a 32 bit output wire representing the instruction being read from the IM
 */
module DP(regDst, jump, jal, beq, bne, memRead,	memToReg,	// establish DP module
 memWrite, ALUSrc, regWrite, shift, jr, ALUConOut,
 instruction);
	input regDst, jump, jal, beq, bne, memRead,	memToReg,	// establish regDst, jump, jal, beq, bne, memRead, memToReg,
		memWrite, ALUSrc, regWrite, shift, jr;				//		memWrite, ALUSrc, regWrite, shift, and jr as 1 bit input wires
	input [4:0] ALUConOut;									// establish ALUConOut as a 5 bit input wire
	output [31:0] instruction;								// establish instruction as a 32 bit output wire
	
	wire clock, zero;										// establish clock and zero as 1 bit wires
	wire [4:0] writeReg, m1Out;								// establish writeReg and m1Out as 5 bit wires
	wire [27:0] slOut;										// establish slOut as a 28 bit wire
	wire [31:0] currentAddress, newAddress,					// establish currentAddress, newAddress,
		extendedAddress, writeData, readData, data1, data2,	//		extendedAddress, writeData, readData, data1, data2,
		ALUIn1, ALUIn2,	ALUResult, m3Out, newHI, newLO, HI,	//		ALUIn1, ALUIn2, ALUResult, m3Out, newHI, newLO, HI,
		LO, addResult, m7Out, m8Out, nextAddress;			//		LO, addResult, m7Out, m8Out, and nextAddress as 32 bit wires
	
	Clock c (clock);										// instantiate a Clock							
	PC pc (clock, nextAddress, currentAddress);				// instantiate a PC
	IM im (clock, currentAddress, instruction);				// instantiate an IM
	DM dm (clock, ALUResult, data2, memWrite, memRead,		// instantiate a DM
		readData);
	RF rf(clock, instruction[25:21], instruction[20:16],	// instantiate an RF
		writeReg, writeData, regWrite, data1, data2);
	SignExtend se (instruction[15:0], extendedAddress);		// instantiate a SignExtend
	HILO hl (clock, newHI, newLO, HI, LO);					// instantiate a HILO
	ALU alu (ALUIn1, ALUIn2, ALUConOut, HI, LO, newHI,		// instantiate an ALU
		newLO, ALUResult, zero);
	ShiftLeft2 sl (instruction[25:0], slOut);				// instantiate a ShiftLeft2
	
	Adder a1 (32'd4, currentAddress, newAddress);			// instantiate an Adder for the PC
	Adder a2 (extendedAddress << 2, newAddress,				// instantiate an Adder for branches
		addResult);
	
	Mux #(5) m1 (regDst, instruction[20:16],				// instantiate two Muxes for determining writeReg
		instruction[15:11], m1Out);
	Mux #(5) m2 (jal, m1Out, 5'd31, writeReg);
	Mux m3 (memToReg, ALUResult, readData, m3Out);			// instantiate two Muxes for determining writeData
	Mux m4 (jal, m3Out, newAddress, writeData);
	Mux m5 (shift, data1, extendedAddress, ALUIn1);			// instantiate a Mux for determining ALUIn1
	Mux m6 (ALUSrc, data2, extendedAddress, ALUIn2);		// instantiate a Mux for determining ALUIn2
	Mux m7 (((beq&zero) | (bne&(~zero))), newAddress,		// instantiate three Muxes for determining nextAddress
		addResult, m7Out);
	Mux m8 (jump, m7Out, {newAddress[31:28], slOut},
		m8Out);
	Mux m9 (jr, m8Out, ALUResult, nextAddress);
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	C					- a module that contains control elements
 *	Ports:
 *			regDst				- a 1 bit output wire that determines which register to use as the write register
 *			jump				- a 1 bit output wire that determines whether a jump is needed
 *			jal					- a 1 bit output wire that determines whether a jal is occurring
 *			beq					- a 1 bit output wire that determines whether a beq is occurring
 *			bne					- a 1 bit output wire that determines whether a bne is occurring
 *			memRead				- a 1 bit output wire that determines whether the memory needs to be read from
 *			memToReg			- a 1 bit output wire that determines whether the information read from memory needs to be written to a register
 *			memWrite			- a 1 bit output wire that determines whether the memory needs to be written to
 *			ALUSrc				- a 1 bit output wire that determines the second input to the ALU
 *			regWrite			- a 1 bit output wire that determines whether the registers need to be written to
 *			shift				- a 1 bit output wire flag indicating whether a shift is occurring
 *			jr					- a 1 bit output wire flag indicating whether a jr is occurring
 *			ALUConOut			- a 5 bit output wire representing the identification of the command being read from instruction
 *			instruction			- a 32 bit output wire representing the instruction being read from the IM
 */
module C(instruction, regDst, jump, jal, beq, bne, memRead,	// establish C module
 memToReg, memWrite, ALUSrc, regWrite, ALUConOut, shift,
 jr);
	input [31:0] instruction;								// establish instruction as a 32 bit input wire
	output regDst, jump, jal, beq, bne, memRead, memToReg,	// establish regDst, jump, jal, beq, bne, memRead, memToReg,
		memWrite, ALUSrc, regWrite, shift, jr;				//		memWrite, ALUSrc, regWrite, shift, and jr as 1 biit output wires
	output [4:0] ALUConOut;									// establish ALUConOut as a 5 bit output wire
	
	wire [3:0] ALUOp;										// establish ALUOp as a 4 bit wire
	
	Control control(instruction[31:26], regDst, jump, jal,	// instantiate a Control
		beq, bne, memRead, memToReg, ALUOp, memWrite,
		ALUSrc, regWrite);
	ALUControl alucontrol (ALUOp, instruction[5:0],			// instantiate an ALUControl
		ALUConOut, shift, jr);
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Clock				- a module that changes its output signal every tick
 *	Ports:
 *			clock				- a 1 bit output reg which toggles between 1 and 0
 */
module Clock(clock);										// establish the Clock module
	output reg clock;										// establish clock as a 1 bit output reg		
		
	always													// forever:
		#1 clock = ~clock;									//		toggle clock between 1 and 0
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	PC					- a module that maintains the current address and adjusts it to the next address during positive clock edges
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			newAddress			- a 32 bit input wire representing the next address that needs to be read
 *			currentAddress		- a 32 bit output reg representing the address that the system is currently on
 */
module PC (clock, newAddress, currentAddress);				// establish the PC module
	input clock;											// establish clock as a 1 bit input wire
	input [31:0] newAddress;								// establish newAddress as a 32 bit input wire
	output reg [31:0] currentAddress;						// establish currentAddress as a 32 bit output reg
	
	always @ (posedge clock)								// during every positive clock tick:
	begin
		#3 currentAddress = newAddress;						// 		set currentAddress to newAddress
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	IM					- a module that reads an instruction from memory based on the current address
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			currentAddress		- a 32 bit input wire representing the address that the system is currently on
 *			instruction			- a 32 bit output wire representing the instruction being read from the IM
 */
module IM (clock, currentAddress, instruction);				// establish the IM module
	input clock;											// establish clock as a 1 bit input wire
	input [31:0] currentAddress;							// establish currentAddress as a 32 bit input wire
	output reg [31:0] instruction;							// establish instruction as a 32 bit output reg
	
	reg [31:0] mem [0:58];									// establish mem as 59 registers, each 32 bits in length
	
	always @(posedge clock)									// during every positive clock tick:
		instruction = mem [currentAddress[7:2]];			//		read data from the currentAddress location in mem into instruction
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	DM					- a module that reads and writes to memory using separate controls for reading and writing (mutually exclusive) and where writing only occurs during positive clock edges
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			address				- a 32 bit input wire representing the address, whose bits will be used in determining the read and write registers
 *			writeData			- a 32 bit input wire representing the data that may be written to memory
 *			memWrite			- a 1 bit input wire flag that determines whether writeData will be written to memory
 *			memRead				- a 1 bit input wire flag that determines whether memory should be read from
 *			readData			- a 32 bit output reg containing the data read from memory 
 */
 module DM (clock, address, writeData, memWrite, memRead,	// establish the DM module
  readData);			
	input clock, memWrite, memRead;							// establish clock, memWrite, and memRead as 1 bit input wires
	input [31:0] address, writeData;						// establish address and writeData as a 32 bit input wires
	output reg [31:0] readData;								// establish readData as a 32 bit output reg
	
	reg [31:0] mem [0:1023];								// establish mem as 1024 registers, each 32 bits in length
	
	always @ (mem[address[10:2]], memRead)					// whenever the current location in mem or memRead is altered:
	begin
		if (memRead)										//		if the memRead flag is triggered:
			readData = mem[address[10:2]];					//			read data from the current location in mem into readData
	end
	
	always @ (posedge clock)								// during every positive clock tick:
	begin
		if (memWrite)										//		if the memWrite flag is triggered:
			mem[address[10:2]] = writeData;					//			write the data from writeData into the current location in mem
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	RF					- a module that will always output the contents of the registers corresponding to the inputs and that will only write during positive clock edges, if provided a signal
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			readReg1			- a 5 bit input wire representing the first register used for reading
 *			readReg2			- a 5 bit input wire representing the second register used for reading
 *			writeReg			- a 5 bit input wire representing the register used for writing
 *			writeData			- a 32 bit input wire representing the data that may be written to writeReg
 *			regWrite			- a 1 bit input wire flag that determines whether writeReg will be written to
 *			data1				- a 32 bit output wire containing the data read from readReg1
 *			data2				- a 32 bit output wire containing the data read from readReg2
 */
module RF (clock, readReg1, readReg2, writeReg,	writeData,	// establish the RF module
 regWrite, data1, data2);
	input clock, regWrite;									// establish clock and regWrite as 1 bit input wires
	input [4:0] readReg1, readReg2, writeReg;				// establish readReg1, readReg2, and writeReg as 5 bit input wires
	input [31:0] writeData;									// establish writeData as a 32 bit input wire
	output [31:0] data1, data2;								// establish data1 and data2 as 32 bit output wires
	
	reg [31:0] mem [0:1023];								// establish mem as 1024 registers, each 32 bits in length
	
	assign data1 = mem[readReg1+991];						// continuously assign data1 to the readReg1 register of mem
	assign data2 = mem[readReg2+991];						// continuously assign data2 to the readReg2 register of mem
	
	always @ (posedge clock)								// during every positive clock tick:
	begin
		#3 if (regWrite)									// 		if the regWrite flag is triggered:
			mem[writeReg+991] = writeData;					//			write the data from writeData into the writeReg register of mem
	end
	
	always @ (mem[991])										// whenever register $0 is altered:
		mem[991] = 0;										//		set register $0 to 0
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	HILO				- a module that stores the HI and LO registers that are utilized by the ALU
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			newHI				- a 32 bit input wire representing the value that the HI register will be changed to on the next clock tick
 *			newLO				- a 32 bit input wire representing the value that the LO register will be changed to on the next clock tick
 *			HI					- a 32 bit output reg representing the HI register
 *			LO					- a 32 bit output reg representing the LO register
 */
module HILO(clock, newHI, newLO, HI, LO);					// establish the HILO module
	input clock;											// establish clock as a 1 bit input wire
	input [31:0] newHI, newLO;								// establish newHI and newLO as 32 bit input wires
	output reg [31:0] HI, LO;								// establish HI and LO as 32 bit output regs
	
	always @ (posedge clock)								// during every positive clock tick:
	begin
		HI = newHI;											//		set the HI register equal to the newHI value
		LO = newLO;											//		set the LO register equal to the newLO value
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	ALU					- a module that performs different operations dependent on its ALUConOut signal and outputs the zero or nonzero result produced by the operation
 *	Ports:
 *			A					- a 32 bit input wire representing the first operand for the ALU operation
 *			B					- a 32 bit input wire representing the second operand for the ALU operation
 *			ALUConOut			- a 5 bit input wire that determines the operation that the ALU will perform
 *			HI					- a 32 bit input wire representing the HI register
 *			LO					- a 32 bit input wire representing the LO register
 *			newHI				- a 32 bit output reg representing the value that the HI register will be changed to on the next clock tick
 *			newLO				- a 32 bit output reg representing the value that the LO register will be changed to on the next clock tick
 *			result				- a 32 bit output reg containing the result of the ALU operation
 *			zero				- a 1 bit output wire containing 1 if the result of the ALU operation was zero and 0 otherwise
 */
module ALU (A, B, ALUConOut, HI, LO, newHI, newLO, result,	// establish the ALU module
 zero);
	input [31:0] A, B, HI, LO;								// establish A, B, HI, and LO as 32 bit input wires
	input [4:0] ALUConOut;									// establish ALUConOut as a 5 bit input wire
	output reg [31:0] result, newHI, newLO;					// establish result, newHI, and newLO as 32 bit output regs
	output zero;											// establish zero as a 1 bit output wire
	
	assign zero = (result == 0);							// zero is set to 1 if result is 0 and 0 otherwise
	
	always @(A, B, ALUConOut)								// whenever A, B, or ALUConOut is altered:
	begin
		case(ALUConOut)										//		use ALUConOut to determine which operation should be performed:
			6'b01010, 6'b00100, 6'b00011: result <= A + B; 	//			if the operation is add, sw, or lw, then A and B are added and stored in result
			6'b01100: result <= A & B; 						//			if the operation is and, then A and B are anded and stored in result
			6'b01100: {newHI, newLO} <= A / B; 				//			if the operation is div, then A and B are divided and stored in newHI and newLO
			6'b01101: {newHI, newLO} <= A * B; 				//			if the operation is mult, then A and B are multiplied and stored in newHI and newLO
			6'b01110: result <= A | B; 						//			if the operation is or, then A and B are ored and stored in result
			6'b01111: result <= B << A[10:6]; 				//			if the operation is sll, then B is shifted left logical based on A and stored in result
			6'b10000: result <= A < B; 						//			if the operation is slt, then if A is less than B, a 1 is stored in result, otherwise a 0 is stored in result 
			6'b10001: result <= B >>> A[10:6]; 				//			if the operation is sra, then B is shifted right arithmetic based on A and stored in result
			6'b10010: result <= B >> A[10:6]; 				//			if the operation is srl, then B is shifted right logical based on A and stored in result
			6'b10011, 6'b00101, 6'b00110: result <= A - B; 	//			if the operation is sub, beq, bne, then the difference of A and B is stored in result
			6'b10100: result <= A; 							//			if the operation is jr, then A is stored in result
			6'b10101: result <= HI; 						//			if the operation is mfhi, then the contents of the HI register is stored in result
			6'b10110: result <= LO; 						//			if the operation is mflo, then the contents of the LO register is stored in result
			6'b00010: result <= A * B; 						//			if the operation is mul, then A and B are multiplied and stored in result
			6'b01001: result <= B << 16; 					//			if the operation is lui, then B is shifted left by 16 and stored in result
			default: result <= 0;							//			otherwise, no operation is performed and the result is set to 0
		endcase
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Control				- a module that outputs the appropriate flags based on the op codes inputted to it
 *	Ports:
 *			opCode				- a 6 bit input wire representing the first six bits (the op code) of an instruction
 *			regDst				- a 1 bit output reg that determines which register to use as the write register
 *			jump				- a 1 bit output reg that determines whether a jump is needed
 *			jal					- a 1 bit output reg that determines whether a jal is occurring
 *			beq					- a 1 bit output reg that determines whether a beq is occurring
 *			bne					- a 1 bit output reg that determines whether a bne is occurring
 *			memRead				- a 1 bit output reg that determines whether the memory needs to be read from
 *			memToReg			- a 1 bit output reg that determines whether the information read from memory needs to be written to a register
 *			ALUOp				- a 2 bit output reg representing the ALUOp that will go to the ALU Control
 *			memWrite			- a 1 bit output reg that determines whether the memory needs to be written to
 *			ALUSrc				- a 1 bit output reg that determines the second input to the ALU
 *			regWrite			- a 1 bit output reg that determines whether the registers need to be written to
 */
module Control (opCode, regDst, jump, jal, beq, bne,		// establish the Control module
 memRead, memToReg, ALUOp, memWrite, ALUSrc, regWrite);
	input [5:0] opCode;										// establish opCode as a 6 bit input wire
	output reg regDst, jump, jal, beq, bne, memRead, 		// establish regDst, jump, jal, beq, bne, mul, memRead,
		memToReg, memWrite, ALUSrc, regWrite;				// 		memToReg, memWrite, ALUSrc, and regWrite as 1 bit output regs
	output reg [3:0] ALUOp;									// establish ALUOp as s 4 bit output reg
	
	always @(opCode)										// every time that opCode changes:
	begin
		case(opCode)
			0: 												// 		op code is 0x00 for:	add, and, div, mult, or, sll, slt, sra, srl, sub, jr, mfhi, mflo, syscall
				begin										//				output flags are set appropriately
					regDst = 1;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0001;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 1;
				end
			
			6'h1c:	 										// 		op code is 0x1c for:	mul
				begin										//				output flags are set appropriately
					regDst = 1;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0010;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 1;
				end
			
			6'h23: 											//		op code is 0x23 for:	lw
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 1;
					memToReg = 1;
					ALUOp = 4'b0011;
					memWrite = 0;
					ALUSrc = 1;
					regWrite = 1;
				end
			
			6'h2b: 											// 		op code is 0x2b for:	sw
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0100;
					memWrite = 1;
					ALUSrc = 1;
					regWrite = 0;
				end
					
			6'h04: 											// 		op code is 0x04 for:	beq
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 0;
					jal = 0;
					beq = 1;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0101;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
			
			6'h05: 											// 		op code is 0x05 for:	bne
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 0;
					jal =0;
					beq = 0;
					bne = 1;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0110;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
					
			6'h02: 											// 		op code is 0x02 for:	j
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 1;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0111;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
					
			6'h03: 											// 		op code is 0x03 for:	jal
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 1;
					jal = 1;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b1000;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 1;
				end

			6'h0f: 											// 		op code is 0x0f for: lui
				begin										//				output flags are set appropriately
					regDst = 0;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b1001;
					memWrite = 0;
					ALUSrc = 1;
					regWrite = 1;
				end
			
			default: 										// 		default case if op code is not one of the ones listed above
				begin										//				output flags are set to 0
					regDst = 0;
					jump = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 0;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
		endcase
	end
endmodule

/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	ALUControl			- a module that outputs a unique output for each command depending on the ALUOp and (possibly) functCode inputted into it
 *	Ports:
 *			ALUOp				- a 4 bit input wire representing the ALUOp outputted from the Control module
 *			functCode			- a 6 bit input wire representing the last six bits (the funct code) of an instruction
 *			ALUConOut			- a 5 bit output reg representing the identification of the command being read from instruction
 *			shift				- a 1 bit output reg flag indicating whether a shift is occurring
 *			jr					- a 1 bit output reg flag indicating whether a jr is occurring
 */
module ALUControl (ALUOp, functCode, ALUConOut, shift, jr);	// establish the ALUControl module
	input [3:0] ALUOp;										// establish ALUOp as a 4 bit input wire
	input [5:0] functCode;									// establish functCode as a 6 bit input wire
	output reg [4:0] ALUConOut;								// establish ALUConOut as a 5 bit output reg
	output reg shift, jr;									// establish shift and jr as 1 bit output regs
	
	always @(ALUOp, functCode)								// every time that either the ALUOp or functCode changes:
	begin
		case(ALUOp)
			4'b0001:										//		if the instruction is an R-type, then the functCode must be checked
			begin
				case(functCode)
					6'h20: 									// 			functCode is 0x20 for:	add
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01010;
						shift = 0;
						jr = 0;
					end
						
					6'h24: 									//			functCode is 0x24 for:	and
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01011;
						shift = 0;
						jr = 0;
					end
					
					6'h1a: 									//			functCode is 0x1a for:	div
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01100;
						shift = 0;
						jr = 0;
					end
					
					6'h18: 									//			functCode is 0x18 for:	mult
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01101;
						shift = 0;
						jr = 0;
					end
					
					6'h25: 									//			functCode is 0x25 for:	or
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01110;
						shift = 0;
						jr = 0;
					end
						
					0:										//			functCode is 0x00 for:	sll
					begin									//				output flags are set appropriately
						ALUConOut = 5'b01111;
						shift = 1;
						jr = 0;
					end
						
					6'h2a:									//			functCode is 0x2a for:	slt
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10000;
						shift = 0;
						jr = 0;
					end
						
					6'h03:									//			functCode is 0x03 for:	sra
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10001;
						shift = 1;
						jr = 0;
					end
						
					6'h02:									//			functCode is 0x02 for:	srl
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10010;
						shift = 1;
						jr = 0;
					end
						
					6'h22:									//			functCode is 0x22 for:	sub
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10011;
						shift = 0;
						jr = 0;
					end
						
					6'h08:									//			functCode is 0x08 for:	jr
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10100;
						shift = 0;
						jr = 1;
					end
						
					6'h10:									//			functCode is 0x10 for:	mfhi
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10101;
						shift = 0;
						jr = 0;
					end
						
					6'h12:									//			functCode is 0x12 for:	mflo
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10110;
						shift = 0;
						jr = 0;
					end
						
					6'h0c:									//			functCode is 0x0c for:	syscall
					begin									//				output flags are set appropriately
						ALUConOut = 5'b10111;
						shift = 0;
						jr = 0;
					end
					
					default:								// 			default case if functCode is not one of the ones listed above
					begin									//				output flags are set to 0
						ALUConOut = 0;
						shift = 0;
						jr = 0;
					end
				endcase
			end
			
			4'b0010, 4'b0011, 4'b0100, 4'b0101,	4'b0110,	// 		for all other instructions: mul, lw, sw, beq, bne, j, jal, lui
			4'b0111, 4'b1000, 4'b1001: 						//			output flags are set appropriately
			begin
				ALUConOut = ALUOp;
				shift = 0;
				jr = 0;
			end
			
			default:										// 		default case if ALUOp is not one of the ones listed above
			begin
				ALUConOut = 0;
				shift = 0;
				jr = 0;
			end
		endcase
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Adder				- a module which calculates the sum of an increment and an addend
 *	Ports:
 *			increment			- a 32 bit input wire representing the value that the addend is being incremented by
 *			addend				- a 32 bit input wire representing the addend that needs to be incremented
 *			sum					- a 32 bit output wire representing the sum of the addend and the increment
 */
module Adder (increment, addend, sum);						// establish the Adder module
	input [31:0] increment, addend;							// establish increment and addend as 32 bit input wires
	output [31:0] sum;										// establish sum as a 32 bit output wire
 
	assign sum = addend + increment;						// continuously assign sum to be the addend plus the increment
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	SignExtend			- a module that extends a 16 bit address to produce a 32 bit address by using the 16th bit of the 16 bit address
 *	Ports:
 *			unextendedAddress	- a 16 bit input wire representing the address that needs to be extended
 *			extendedAddress		- a 32 bit output wire representing the extended address
 */
module SignExtend(unextendedAddress, extendedAddress);		// establish the SignExtend module
	input [15:0] unextendedAddress;							// establish unextendedAddress as a 16 bit input wire
	output [31:0] extendedAddress;							// establish extendedAddress as a 32 bit input wire
	
	assign extendedAddress =								// continuously assign extendedAddress to be the 16th bit of unextendedAddress repeated 16 times and prefixed onto unextendedAddress
		{{16{unextendedAddress[15]}}, unextendedAddress};
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	ShiftLeft2			- a module that shifts its input to the left twice by concatenating it with two zeros at the end
 *	Ports:
 *			in					- a 26 bit input wire representing the non-shifted input
 *			out					- a 28 bit output wire representing the input shifted left twice
 */
module ShiftLeft2(in, out);									// establish the ShiftLeft2 module
	input [25:0] in;										// establish in as a 26 bit input wire
	output [27:0] out;										// establish out as a 28 bit input wire
	
	assign out = {in, 2'b00};								// continuously assign out to be the concatenation of in with two zeros at the end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Mux					- a module that selects one of two inputs inputted to it depending on the selector bit also inputted to it
 *	Parameters:
 *			n					- a value representing the amount of bits for the two inputs and the output, default is 32
 *	Ports:
 *			sel					- a 2 bit input wire representing the selector bit
 *			in1					- an n bit input wire representing the first input that could be selected
 *			in2					- an n bit input wire representing the second input that could be selected
 *			out					- an n bit output reg representing the output that was selected
 */
module Mux(sel, in1, in2, out);								// establish the Mux module
	parameter n = 32;										// establish n as a parameter with a value of 32
	input sel;												// establish sel as a 1 bit input wire
	input [n-1:0] in1, in2;									// establish in1 and in2 as n bit input wires
	output reg [n-1:0] out;									// establish out as an n bit output reg
	always @(sel, in1, in2)									// whenever sel, in1, or in2 is altered:
	begin
		case(sel)
			0: 			out <= in1;							//		if the selector bit is 0, then out is set to in1
			1: 			out <= in2;							//		if the selector bit is 1, then out is set to in2
			default:	out <= 0;							//		otherwise out is set to 0
		endcase
	end
endmodule