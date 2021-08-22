/*
 *  File: 		SimpleDataPathWithControlAndALUControl.v
 *
 *  Description:
 *				Takes the simple data path created in the file SimpleDataPathWithControl.v and implements an ALU control component to it.
 *
 *	Modules:
 *				testBench	- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *				Clock		- a module that changes the output signal every tick
 *				PC			- a module that adjusts the current address to the next address during every positive clock edge
 *				IM			- a module that reads and translates the current address  to read data from memory and/or write data to memory
 *				Adder		- a module which calculates the next address by incrementing the current address
 * 				Control		- a module that outputs differing flags based on the op codes inputted to it
 *				ALUControl	- a module that outputs a unique output for each command depending on the ALUOp and (possibly) the functCode inputted into it
 */

 
/*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	testBench		- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *	Ports:	None
 */
module testBench;								// establish the testBench module
	wire clock, regDst, j, jal, beq, bne, mul,	// establish clock, regDst, j, jal, beq, bne, mul, memRead, memToReg, memWrite, ALUSrc, and regWrite as 1 bit wires
		memRead, memToReg, memWrite, ALUSrc,
		regWrite;
	wire [3:0] ALUOp;							// establish ALUOp as a 4 bit wire
	wire [4:0] ALUOutput;						// establish ALUOutput as a 5 bit wire
	wire [31:0] currentAddress, newAddress,		// establish currentAddress, newAddress, and instruction as 32 bit wires
		instruction;
	
	Clock c (clock);							// instantiate a Clock							
	PC pc (clock, newAddress, currentAddress);	// instantiate a PC
	IM im (clock, currentAddress, instruction);	// instantiate an IM
	Adder adder (3'd4, currentAddress,			// instantiate an Adder
		newAddress);
	Control ctrl (instruction[31:26], regDst,	// instantiate a Control
		j, jal, beq, bne, mul, memRead,
		memToReg, ALUOp, memWrite, ALUSrc,
		regWrite);
	ALUControl aluCtrl (ALUOp, instruction[5:0], ALUOutput);
	
	initial										// initially:
	begin
		$readmemh("IMmem.txt", im.mem);			//		set up the memory of IM
		
		c.clock = 0;							// 		initial input values for testing
		pc.currentAddress = 0;
												// 		begin testing add
		$display("////////////\nTESTING ADD:\n////////////");
												//		prints the following output whenever the mentioned variables are altered (for testing)
		$monitor ("clock=%d, address=%h, instruction=%h, opcode=%h, functCode=%h, regDst=%b, j=%b, jal=%b, beq=%b, bne=%b, mul=%b, memRead=%b, memToReg=%b, ALUOp=%b, memWrite=%b, ALUSrc=%b, regWrite=%b, ALUOutput=%b\n",
			clock, currentAddress, instruction, instruction[31:26], instruction[5:0], regDst, j, jal, beq, bne, mul, memRead, memToReg, ALUOp, memWrite, ALUSrc, regWrite, ALUOutput);
												// 		begin testing and
		#3 $display("////////////\nTESTING AND:\n////////////");
												// 		begin testing div
		#4 $display("////////////\nTESTING DIV:\n////////////");
												// 		begin testing mult
		#4 $display("/////////////\nTESTING MULT:\n/////////////");
												// 		begin testing or
		#4 $display("///////////\nTESTING OR:\n///////////");
												// 		begin testing sll
		#4 $display("////////////\nTESTING SLL:\n////////////");
												// 		begin testing slt
		#4 $display("////////////\nTESTING SLT:\n////////////");
												// 		begin testing sra
		#4 $display("////////////\nTESTING SRA:\n////////////");
												// 		begin testing srl
		#4 $display("////////////\nTESTING SRL:\n////////////");
												// 		begin testing sub
		#4 $display("////////////\nTESTING SUB:\n////////////");
												// 		begin testing jr
		#4 $display("///////////\nTESTING JR:\n///////////");
												// 		begin testing mfhi
		#4 $display("/////////////\nTESTING MFHI:\n/////////////");
												// 		begin testing mflo
		#4 $display("/////////////\nTESTING MFLO:\n/////////////");
												// 		begin testing syscall
		#4 $display("/////////////////\nTESTING SYSCALL:\n/////////////////");
												// 		begin testing mul
		#4 $display("////////////\nTESTING MUL:\n////////////");
												// 		begin testing lw
		#4 $display("///////////\nTESTING LW:\n///////////");
												// 		begin testing sw
		#4 $display("///////////\nTESTING SW:\n///////////");
												// 		begin testing beq
		#4 $display("////////////\nTESTING BEQ:\n////////////");
												// 		begin testing bne
		#4 $display("////////////\nTESTING BNE:\n////////////");
												// 		begin testing j
		#4 $display("//////////\nTESTING J:\n//////////");
												// 		begin testing jal
		#4 $display("////////////\nTESTING JAL:\n////////////");
												// 		begin testing lui
		#4 $display("////////////\nTESTING LUI:\n////////////");
		#3 $finish;								// 		end the program
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
	
	reg [31:0] mem [0:63];							// establish mem as 64 registers, each 32 bits in length
	
	always @(posedge clock)							// during every positive clock tick:
		instruction = mem [currentAddress[7:2]];	//		read instruction from the currentAddress location in mem into instruction
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
 *			j				- a 1 bit output reg representing the j flag
 *			jal				- a 1 bit output reg representing the jal flag
 *			beq				- a 1 bit output reg representing the beq flag
 *			bne				- a 1 bit output reg representing the bne flag
 *			mul				- a 1 bit output reg representing the mul flag
 *			memRead			- a 1 bit output reg representing the memRead flag
 *			memToRead		- a 1 bit output reg representing the memToRead flag
 *			ALUOp			- a 2 bit output reg representing the ALUOp that will go to the ALU Control
 *			memWrite		- a 1 bit output reg representing the memWrite flag
 *			ALUSrc			- a 1 bit output reg representing the ALUSrc flag
 *			regWrite		- a 1 bit output reg representing the regWrite flag
 */
module Control (opCode, regDst, j, jal, beq, bne, mul,	// establish the Control module
   memRead, memToReg, ALUOp, memWrite, ALUSrc, regWrite);
	input [5:0] opCode;									// establish opCode as a 6 bit input wire
	output reg regDst, j, jal, beq, bne, mul, memRead, 	// establish regDst, j, jal, beq, bne, mul, memRead, memToReg, memWrite, ALUSrc, and regWrite as 1 bit output regs
		memToReg, memWrite, ALUSrc, regWrite;
	output reg [3:0] ALUOp;								// establish ALUOp as s 4 bit output reg
	
	always @(opCode)									// every time that opCode changes:
	begin
		case(opCode)
			0: 											// 		op code is 0x00 for:	add, and, div, mult, or, sll, slt, sra, srl, sub, jr, mfhi, mflo, syscall
				begin									//				output flags are set appropriately
					regDst = 1;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0001;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 1;
				end
			
			6'h1c:	 									// 		op code is 0x1c for:	mul
				begin									//				output flags are set appropriately
					regDst = 1;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 1;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0010;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 1;
				end
			
			6'h23: 										//		op code is 0x23 for:	lw
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 1;
					memToReg = 1;
					ALUOp = 4'b0011;
					memWrite = 0;
					ALUSrc = 1;
					regWrite = 1;
				end
			
			6'h2b: 										// 		op code is 0x2b for:	sw
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0100;
					memWrite = 1;
					ALUSrc = 1;
					regWrite = 0;
				end
					
			6'h04: 										// 		op code is 0x04 for:	beq
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 0;
					jal = 0;
					beq = 1;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0101;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
			
			6'h05: 										// 		op code is 0x05 for:	bne
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 0;
					jal =0;
					beq = 0;
					bne = 1;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0110;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
					
			6'h02: 										// 		op code is 0x02 for:	j
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 1;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b0111;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end
					
			6'h03: 										// 		op code is 0x03 for:	jal
				begin									//				output flags are set appropriately
					regDst = 0;
					j = 0;
					jal = 1;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b1000;
					memWrite = 0;
					ALUSrc = 0;
					regWrite = 0;
				end

			6'h0f: 										// 		op code is 0x0f for: lui
				begin									//				output flags are set appropriately
					regDst = 1;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
					memRead = 0;
					memToReg = 0;
					ALUOp = 4'b1001;
					memWrite = 0;
					ALUSrc = 1;
					regWrite = 1;
				end
			
			default: 									// 		default case if op code is not one of the ones listed above
				begin									//				output flags are set to 0
					regDst = 0;
					j = 0;
					jal = 0;
					beq = 0;
					bne = 0;
					mul = 0;
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
 *  Module:	ALUControl		- a module that outputs a unique output for each command depending on the ALUOp and (possibly) the functCode inputted into it
 *	Ports:
 *			ALUOp			- a 4 bit input wire representing the ALUOp outputted from the Control module
 *			functCode		- a 6 bit input wire representing the last six bits (the funct code) of an instruction
 *			ALUOutput		- a 5 bit output reg representing the identification of the command being read from instruction
 */
module ALUControl (ALUOp, functCode, ALUOutput);		// establish the ALUControl module
	input [3:0] ALUOp;									// establish ALUOp as a 4 bit input wire
	input [5:0] functCode;								// establish functCode as a 6 bit input wire
	output reg [4:0] ALUOutput;							// establish ALUOutput as a 5 bit output reg
	
	always @(ALUOp, functCode)							// every time that either the ALUOp or functCode changes:
	begin
		case(ALUOp)
			4'b0001:									//		if ALUOp is 1, the instruction is an R-type instruction and the functCode must be checked
			begin
				case(functCode)
					6'h20: 								// 			functCode is 0x20 for:	add
						ALUOutput = 5'b01010;
						
					6'h24: 								//			functCode is 0x24 for:	and
						ALUOutput = 5'b01011;
					
					6'h1a: 								//			functCode is 0x1a for:	div
						ALUOutput = 5'b01100;
					
					6'h18: 								//			functCode is 0x18 for:	mult
						ALUOutput = 5'b01101;
					
					6'h25: 								//			functCode is 0x25 for:	or
						ALUOutput = 5'b01110;
						
					0:									//			functCode is 0x00 for:	sll
						ALUOutput = 5'b01111;
						
					6'h2a:								//			functCode is 0x2a for:	slt
						ALUOutput = 5'b10000;
						
					6'h03:								//			functCode is 0x03 for:	sra
						ALUOutput = 5'b10001;
						
					6'h02:								//			functCode is 0x02 for:	srl
						ALUOutput = 5'b10010;
						
					6'h22:								//			functCode is 0x22 for:	sub
						ALUOutput = 5'b10011;
						
					6'h08:								//			functCode is 0x08 for:	jr
						ALUOutput = 5'b10100;
						
					6'h10:								//			functCode is 0x10 for:	mfhi
						ALUOutput = 5'b10101;
						
					6'h12:								//			functCode is 0x12 for:	mflo
						ALUOutput = 5'b10110;
						
					6'h0c:								//			functCode is 0x0c for:	syscall
						ALUOutput = 5'b10111;
					
					default:							// 			default case if functCode is not one of the ones listed above
						ALUOutput = 0;
				endcase
			end
			
			4'b0010, 4'b0011, 4'b0100, 4'b0101,			// 		for all other unique instructions, set ALUOutput to ALUOp
			 4'b0110, 4'b0111, 4'b1000, 4'b1001:
				ALUOutput = ALUOp;
			
			default:									// 		default case if ALUOp is not one of the ones listed above
				ALUOutput = 0;
		endcase
	end
endmodule