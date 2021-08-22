/*
 *  File: 		RegistersDataPath.v
 *
 *	Description:
 *				Implementation of a data path with four components (register file, ALU, data memory unit, and sign extension unit).
 *
 *	Modules:
 *				testBench		- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *				Clock			- a module that changes the output signal every tick
 *				Registers		- a module that will always output the contents of the registers corresponding to the inputs, and will only write during positive clock edges if provided a signal
 *				ALU				- a module that performs different operations dependent on its op signal and outputs the zero or nonzero result produced by the operation
 *				DM				- a module that has separate controls for reading and writing such that only one can occur at a given time and where writing only occurs during positive clock edges
 *				SignExtend		- a module that extends a 16 bit address to produce a 32 bit address by using the 16th bit of the 16 bit address
 */

 
/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	testBench			- a module for testing whether the program functions by printing to the output
 *	Ports:	None
 */
module testBench;										// establish the testBench module
	wire clock, zero;									// establish clock and zero as 1 bit wires
	wire [31:0] readData, data1, data2, result,			// establish readData, data1, data2, result, and extendedAddress as 32 bit wires
	 extendedAddress;
	 
	reg [31:0] address, writeData, A, B;				// establish address, writeData, A, and B as 32 bit regs
	reg [15:0] unextendedAddress;						// establish unextendedAddress as a 16 bit reg
	reg [4:0] readReg1, readReg2, writeReg;				// establish readReg1, readReg2, and writeReg as 5 bit regs
	reg [3:0] op;										// establish op as a 4 bit reg
	reg memWrite, memRead, regWrite;					// establish memWrite, memRead, and regWrite as 1 bit regs
	
	Clock c (clock);									// instantiate a Clock		
	Registers r(clock, readReg1, readReg2,				// instantiate a Registers
	 writeReg, writeData, regWrite, data1, data2);
	ALU alu(A, B, op, result, zero);					// instantiate a ALU
	DM dm (clock, address, writeData,					// instantiate a DM
	 memWrite, memRead, readData);
	SignExtend se(unextendedAddress, extendedAddress);	// instantiate a SignExtend
	
	initial												// initially:
	begin
		$readmemh("DMmem.txt", dm.mem);					// 		set up the memory of the DM
		$readmemh("Rmem.txt", r.mem);					//		set up the memory of the Registers
		
		c.clock = 0;									//		initial input values for testing Registers	
		readReg1 = 0;
		readReg2 = 0;
		writeData = 0;
		writeReg = 0;
		regWrite = 0;
		
		A = 0;											// 		initial input values for testing ALU
		B = 0;
		op = 0;
		
		address = 0;									// 		initial input values for testing DM
		memWrite = 0;
		memRead = 0;
		
		unextendedAddress = 0;							// 		initial input values for testing SignExtend
		
														// 		begin testing Registers
		$display("//////////////////\nTESTING REGISTERS:\n//////////////////");
														// 		prints the following output whenever the mentioned variables are altered (for testing)
		$monitor ("R:   clock=%d, readReg1=%d, readReg2=%d, writeReg=%d, writeData=%h, regWrite=%d, data1=%h, data2=%h\nALU: A=%h, B=%h, op=%d, result=%h, zero=%d\nDM:  clock=%d, address=%h, writeData=%h, memWrite=%d, memRead=%d, readData=%h\nSE:  unextendedAddress=%h, extendedAddress=%h\n\n",
		   clock, readReg1, readReg2, writeReg, writeData, regWrite, data1, data2, A, B, op, result, zero, clock, address, writeData, memWrite, memRead, readData, unextendedAddress, extendedAddress);
		
		#2 writeData = 32'h11111111;					//		alter writeData			; output should be unaffected
		#2 writeReg = 1;								//		set writeReg to 1		; output should be unaffected
		#2 regWrite = 1;								//		set regWrite to 1		; output should be unaffected
		#2 readReg1 = 1;								//		set readReg1 to 1		; data1 should display the value stored in writeData
		#2 writeData = 32'h22222222;					// 		alter writeData			; data1 should display the value stored in writeData when clock = 1
		#2 writeReg = 2;								//		set writeReg to 2		; output should be unaffected
		#2 readReg2 = 2;								//		set readReg2 to 2		; data2 should display the value stored in writeData
		#2 regWrite = 0;								// 		set regWrite to 0		; output should be unaffected
		#2 writeData = 32'h33333333;					// 		alter writeData			; output should be unaffected
		
														// begin testing ALU
		#2 $display("////////////\nTESTING ALU:\n////////////");
		#1 A = 10;										// 		set A to 10				; output should be unaffected
		#1 B = 10;										// 		set B to 10				; result should become 10 and zero should become 0
		#1 op = 1;										//		set op to 1				; output should be unaffected
		#1 A = 0;										//		set A to 0				; output should be unaffected
		#1 B = 0;										//		set B to 0				; result should become 0 and zero should become 1
		#1 op = 2;										// 		set op to 2				; output should be unaffected
		#1 A = 1;										//		set A to 1				; result should become 1 and zero should become 0
		#1 op = 6;										//		set op to 6				; output should be unaffected
		#1 A = 0;										//		set A to 0				; result should become 0 and zero should become 1
		#1 op = 7;										//		set op to 7				; output should be unaffected
		#1 B = 32'hffffffff;							//		set B to 32'hffffffff	; result should become 1 and zero should become 0
		#1 op = 12;										// 		set op to 12			; result should become 0 and zero should become 1
		#1 B = 0;										// 		set B to 0				; result should become h'ffffffff and zero should become 0
		#1 op = 13;										//		set op to 13			; result should become 0 and zero should become 1
		#1 B = 10;										// 		set B to 10				; output should be unaffected
		
														// begin testing DM
		#2 $display("////////////////////\nTESTING DATA MEMORY:\n////////////////////");
		#2 memRead = 1;									// 		set memRead to 1		; readData should display the value stored in the address location of memory (0)
		#2 memRead = 0;									// 		set memRead to 0		; output should be unaffected
		memWrite = 1;									// 		set memWrite to 1		; output should be unaffected
		#2 memWrite = 0;								// 		set memWrite to 0		; output should be unaffected
		memRead = 1;									// 		set memRead to 1		; readData should display the value stored in the address location of memory (32'h33333333)
		#2 address = 32'hffffffff;						//		alter address			; readData should display the value stored in the address location of memory (0)
		#2 writeData = 32'h44444444;					//		alter writeData			; output should be unaffected
		#2 memRead = 0;									//		set memRead to 0		; output should be unaffected
		memWrite = 1;									//		set memWrite to 1		; output should be unaffected
		#2 memWrite = 0;								//		set memWrite to 0		; output should be unaffected
		memRead = 1;									//		set memRead to 1		; readData should display the value stored in the address location of memory (32'h44444444)
		
		#2 $display("////////////////////\nTESTING SIGN-EXTEND:\n////////////////////");
		#1 unextendedAddress = 16'h1234;				//		alter unextendedAddress	; extendedAddress should be altered
		#1 unextendedAddress = 16'habcd;				//		alter unextendedAddress	; extendedAddress should be altered
		
		$finish;										// 		end the program
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Clock				- a module that changes the output signal every tick
 *	Ports:
 *			clock				- a 1 bit output reg which toggles between 1 and 0
 */
module Clock(clock);							// establish the Clock module
	output reg clock;							// establish clock as an output reg		
		
	always										// forever:
		#1 clock = ~clock;						//		toggle clock between 1 and 0
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Registers		- a module that will always output the contents of the registers corresponding to the inputs, and will only write during positive clock edges if provided a signal
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
module Registers (clock, readReg1, readReg2,	// establish the Registers module
 writeReg, writeData, regWrite, data1, data2);			
	input [4:0] readReg1, readReg2, writeReg;	// establish readReg1, readReg2, and writeReg as 5 bit input wires
	input [31:0] writeData;						// establish writeData as a 32 bit input wire
	input regWrite, clock;						// establish regWrite and clock as 1 bit input wires
	output [31:0] data1, data2;					// establish data1 and data2 as 32 bit output wires
	
	reg [31:0] mem [0:31];						// establish mem as 32 registers, each 32 bits in length
	
	assign data1 = mem[readReg1];				// continuously assign data1 to the readReg1 register of mem
	assign data2 = mem[readReg2];				// continuously assign data2 to the readReg2 register of mem
	
	always @ (posedge clock)					// during every positive clock tick:
	begin
		if (regWrite)							// 		if the regWrite flag is triggered:
			mem[writeReg] = writeData;			//			write the data from writeData into the writeReg register of mem
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	ALU				- a module that performs different operations dependent on its op signal and outputs the zero or nonzero result produced by the operation
 *	Ports:
 *			A					- a 32 bit input wire representing the first operand for the ALU operation
 *			B					- a 32 bit input wire representing the second operand for the ALU operation
 *			op					- a 4 bit input wire that determines which operation the ALU will performs
 *			result				- a 32 bit output reg containing the result of the ALU operation
 *			zero				- a 1 bit output wire containing 1 if the result of the ALU operation was zero and 0 otherwise
 */
module ALU (A, B, op, result, zero);			// establish the ALU module
	input [31:0] A, B;							// establish A and B as 32 bit input wires
	input [3:0] op;								// establish op as a 4 bit input wire
	output reg [31:0] result;					// establish result as a 32 bit output reg
	output zero;								// establish zero as a 1 bit output wire
	
	assign zero = (result == 0);				// zero is set to 1 if result is 0 and 0 otherwise
	
	always @(A, B, op)							// whenever A, B, or op is altered:
	begin
		case(op)								//		use op to determine which operation should be performed:
			0: result <= A & B;					//			if op is 0, perform an AND operation
			1: result <= A | B;					//			if op is 1, perform an OR operation
			2: result <= A + B;					//			if op is 0, perform an ADD operation
			6: result <= A - B;					//			if op is 0, perform a SUBTRACT operation
			7: result <= A < B ? 1 : 0;			//			if op is 0, perform a LESS THAN operation
			12: result <= ~(A | B);				//			if op is 0, perform a NOR operation
			default: result <= 0;				//			otherwise, no operation is performed and the result is set to 0
		endcase
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	DM				- a module that has separate controls for reading and writing such that only one can occur at a given time and where writing only occurs during positive clock edges
 *	Ports:
 *			clock				- a 1 bit input wire which toggles between 1 and 0
 *			address				- a 32 bit input wire representing the address, whose bits will be used in determining the read and write registers
 *			writeData			- a 32 bit input wire representing the data that may be written to memory
 *			memWrite			- a 1 bit input wire flag that determines whether writeData will be written to memory
 *			memRead				- a 1 bit input wire flag that determines whether memory should be read from
 *			readData			- a 32 bit output reg containing the data read from memory 
 */
 module DM (clock, address, writeData,			// establish the DM module
  memWrite, memRead, readData);			
	input clock;								// establish clock as a 1 bit input wire
	input [31:0] address, writeData;			// establish address and writeData as a 32 bit input wires
	input memWrite, memRead;					// establish memWrite and memRead as a 1 bit input wires
	output reg [31:0] readData;					// establish readData as a 32 bit output reg
	
	reg [31:0] mem [0:31];						// establish mem as 32 registers, each 32 bits in length
	
	always @ (mem[address[6:2]], memRead)		// whenever the current location in mem or memRead is altered:
	begin
		if (memRead)							//		if the memRead flag is triggered:
			readData = mem[address[6:2]];		//			read data from the current location in mem into readData
	end
	
	always @ (posedge clock)					// during every positive clock tick:
	begin
		if (memWrite)							//		if the memWrite flag is triggered:
			mem[address[6:2]] = writeData;		//			write the data from writeData into the current location in mem
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	SignExtend			- a module that extends a 16 bit address to produce a 32 bit address by using the 16th bit of the 16 bit address
 *	Ports:
 *			unextendedAddress	- a 16 bit input wire representing the address that needs to be extended
 *			extendedAddress		- a 32 bit output wire representing the extended address
 */
module SignExtend(unextendedAddress, extendedAddress);							// establish the SignExtend module
	input [15:0] unextendedAddress;												// establish unextendedAddress as a 16 bit input wire
	output [31:0] extendedAddress;												// establish extendedAddress as a 32 bit input wire
	
	assign extendedAddress = {{16{unextendedAddress[15]}}, unextendedAddress};	// continuously assign extendedAddress to be the 16th bit of unextendedAddress repeated 16 times and prefixed onto unextendedAddress
endmodule