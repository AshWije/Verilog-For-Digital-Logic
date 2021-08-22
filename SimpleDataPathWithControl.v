/*
 *  File: 		SimpleDataPathWithControl.v
 *
 *  Description:
 *				Takes the simple data path created in the file SimpleDataPath.v and implements a control component to it.
 *
 *	Modules:
 *				testBench	- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *				Clock		- a module that changes the output signal every tick
 *				PC			- a module that adjusts the current address to the next address during every positive clock edge
 *				IM			- a module that reads and translates the current address  to read data from memory and/or write data to memory
 *				Adder		- a module which calculates the next address by incrementing the current address
 */

 
/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	testBench		- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *	Ports:	None
 */
module testBench;								// establish the testBench module
	wire clock, regDst, jump, branch, memRead,	// establish clock, regDst, jump, branch, memRead, memToReg, memWrite, ALUSrc, and regWrite as 1 bit wires
		memToReg, memWrite, ALUSrc, regWrite;
	wire [1:0] ALUOp;							// establish ALUOp as a 2 bit wire
	wire [31:0] currentAddress, newAddress,		// establish currentAddress, newAddress, and instruction as 32 bit wires
		instruction;
	
	Clock c (clock);							// instantiate a Clock							
	PC pc (clock, newAddress, currentAddress);	// instantiate a PC
	IM im (clock, currentAddress, instruction);	// instantiate an IM
	Adder adder (3'd4, currentAddress,			// instantiate an Adder
		newAddress);
	Control ctrl (instruction[31:26], regDst,	// instantiate a Control
		jump, branch, memRead, memToReg, ALUOp,
		memWrite, ALUSrc, regWrite);
	
	initial										// initially:
	begin
		$readmemh("IMmem.txt", im.mem);			//		set up the memory of IM
		
		c.clock = 0;							// 		initial input values for testing
		pc.currentAddress = 0;
												// 		begin testing using op code 0x00 from the IM memory
		$display("/////////////////////////////////////////////////////////////////////////\nTESTING ADD, AND, DIV, MULT, OR, SLL, SLT, SRA, SRL, SUB, JR, MFHI, MFLO:\n/////////////////////////////////////////////////////////////////////////");
												// 		prints the following output whenever the mentioned variables are altered (for testing)
		$monitor ("clock=%d, address=%h, instruction=%h, opcode=%h, regDst=%b, jump=%b, branch=%b, memRead=%b, memToReg=%b, ALUOp=%b, memWrite=%b, ALUSrc=%b, regWrite=%b\n",
			clock, currentAddress, instruction, instruction[31:26], regDst, jump, branch, memRead, memToReg, ALUOp, memWrite, ALUSrc, regWrite);
		
												// 		begin testing using op code 0x1c from the IM memory
		#4 $display("////////////\nTESTING MUL:\n////////////");
												// 		begin testing using op code 0x23 from the IM memory
		#4 $display("///////////\nTESTING LW:\n///////////");
												// 		begin testing using op code 0x2b from the IM memory
		#4 $display("///////////\nTESTING SW:\n///////////");
												// 		begin testing using op code 0x04 from the IM memory
		#4 $display("////////////\nTESTING BEQ:\n////////////");
												// 		begin testing using op code 0x05 from the IM memory
		#4 $display("////////////\nTESTING BNE:\n////////////");
												// 		begin testing using op code 0x02 from the IM memory
		#4 $display("//////////\nTESTING J:\n//////////");
												// 		begin testing using op code 0x03 from the IM memory
		#4 $display("////////////\nTESTING JAL:\n////////////");
												// 		begin testing using op code 0x0f from the IM memory
		#4 $display("////////////\nTESTING LUI :\n////////////");
		#4 $finish;								// 		end the program
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Clock				- a module that changes the output signal every tick
 *	Ports:
 *			clock				- a 1 bit output reg which toggles between 1 and 0
 */
module Clock(clock);							// establish the Clock module
	output reg clock;							// establish clock as a 1 bit output reg		
		
	always										// forever:
		#1 clock = ~clock;						//		toggle clock between 1 and 0
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	PC				- a module that adjusts the current address to the next address during every positive clock edge
 *	Ports:
 *			clock			- a 1 bit input wire which toggles between 1 and 0
 *			newAddress		- a 32 bit input wire representing the next address that needs to be read
 *			currentAddress	- a 32 bit output reg representing the address that the system is currently on
 */
module PC (clock, newAddress, currentAddress);	// establish the PC module
	input clock;								// establish clock as a 1 bit input wire
	input [31:0] newAddress;					// establish newAddress as a 1 bit input wire
	output reg [31:0] currentAddress;			// establish currentAddress as a 1 bit output reg
	
	always @ (posedge clock)					// during every positive clock tick:
		currentAddress = newAddress;			// 		set currentAddress to newAddress
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	IM				- a module that uses the current address to read or write data from memory
 *	Ports:
 *			clock			- a 1 bit input wire which toggles between 1 and 0
 *			currentAddress	- a 32 bit input wire representing the address that the system is currently on
 *			data			- a 32 bit output wire representing the data being read from the IM
 */
module IM (clock, currentAddress, instruction);		// establish the IM module			
	input clock;									// establish clock as a 1 bit input wire
	input [31:0] currentAddress;					// establish currentAddress as a 32 bit input wire
	output reg [31:0] instruction;					// establish instruction as a 32 bit output reg
	
	reg [31:0] mem [0:31];							// establish mem as 32 registers, each 32 bits in length
	
	always @(posedge clock)							// during every positive clock tick:
		instruction = mem [currentAddress[11:2]];	//		read instruction from the currentAddress location in mem into instruction
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Adder			- a module which calculates the next address by incrementing the current address by an increment
 *	Ports:
 *			increment		- a 3 bit input wire representing the value that currentAddress is being incremented by
 *			currentAddress	- a 32 bit input wire representing the address that the system is currently on
 *			newAddress		- a 32 bit output wire representing the next address that needs to be read
 */
module Adder (increment, currentAddress, newAddress);	// establish the Adder module
	input [2:0] increment;								// establish increment as a 3 bit input wire
	input [31:0] currentAddress;						// establish currentAddress as a 32 bit input wire
	output [31:0] newAddress;							// establish newAddress as a 32 bit output wire
 
	assign newAddress = currentAddress + increment;		// continuously assign newAddress to be the currentAddress plus four (the next address)
endmodule

/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Control			- a module that outputs differing flags based on the op codes inputted to it
 *	Ports:
 *			opCode			- a 6 bit input wire representing the first six bits (the op code) of an instruction
 *			regDst			- a 1 bit output reg representing the regDst flag
 *			jump			- a 1 bit output reg representing the jump flag
 *			branch			- a 1 bit output reg representing the branch flag
 *			memRead			- a 1 bit output reg representing the memRead flag
 *			memToRead		- a 1 bit output reg representing the memToRead flag
 *			ALUOp			- a 2 bit output reg representing the ALUOp that will go to the ALU Control
 *			memWrite		- a 1 bit output reg representing the memWrite flag
 *			ALUSrc			- a 1 bit output reg representing the ALUSrc flag
 *			regWrite		- a 1 bit output reg representing the regWrite flag
 */
module Control (opCode, regDst, jump, branch, memRead,	// establish the Control module
   memToReg, ALUOp, memWrite, ALUSrc, regWrite);
	input [5:0] opCode;									// establish opCode as a 6 bit input wire
	output reg regDst, jump, branch, memRead, memToReg,	// establish regDst, jump, branch, memRead, memToReg, memWrite, ALUSrc, and regWrite as 1 bit output regs
		memWrite, ALUSrc, regWrite;
	output reg [1:0] ALUOp;								// establish ALUOp as s 2 bit output reg
	
	always @(opCode)									// every time that opCode changes:
	begin
		case(opCode)
			0, 6'h1c: 									// 		op code is 0x00 for:	add, and, div, mult, or, sll, slt, sra, srl, sub, jr, mfhi, mflo
					begin								//		op code is 0x1c for:	mul
						regDst = 1;						//				output flags are set appropriately
						jump = 0;
						branch = 0;
						memRead = 0;
						memToReg = 0;
						ALUOp = 2'b10;
						memWrite = 0;
						ALUSrc = 0;
						regWrite = 1;
					end
			
			6'h23: 										//		op code is 0x23 for:	lw
					begin
						regDst = 0;						//				output flags are set appropriately
						jump = 0;
						branch = 0;
						memRead = 1;
						memToReg = 1;
						ALUOp = 0;
						memWrite = 0;
						ALUSrc = 1;
						regWrite = 1;
					end
			
			6'h2b: 										// 		op code is 0x2b for:	sw
					begin
						regDst = 0;						//				output flags are set appropriately
						jump = 0;
						branch = 0;
						memRead = 0;
						memToReg = 0;
						ALUOp = 0;
						memWrite = 1;
						ALUSrc = 1;
						regWrite = 0;
					end
					
			6'h04, 6'h05: 								// 		op code is 0x04 for:	beq
					begin								//		op code is 0x05 for:	bne
						regDst = 0;						//				output flags are set appropriately
						jump = 0;
						branch = 1;
						memRead = 0;
						memToReg = 0;
						ALUOp = 2'b01;
						memWrite = 0;
						ALUSrc = 0;
						regWrite = 0;
					end
					
			6'h02, 6'h03: 								// 		op code is 0x02 for:	j
					begin								//		op code is 0x03 for:	jal
						regDst = 0;						//				output flags are set appropriately
						jump = 1;
						branch = 0;
						memRead = 0;
						memToReg = 0;
						ALUOp = 0;
						memWrite = 0;
						ALUSrc = 0;
						regWrite = 0;
					end

			6'h0f: 										// 		op code is 0x0f for: lui
					begin
						regDst = 1;						//				output flags are set appropriately
						jump = 0;
						branch = 0;
						memRead = 0;
						memToReg = 0;
						ALUOp = 0;
						memWrite = 0;
						ALUSrc = 1;
						regWrite = 1;
					end
			
			default: 									// 		default case if op code is not one of the ones listed above
					begin
						regDst = 0;						//				output flags are set appropriately
						jump = 0;
						branch = 0;
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