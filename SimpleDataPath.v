/*
 *  File: 		SimpleDataPath.v
 *
 *	Description:
 *				Implementation of a simple data path with four components (program counter, instruction memory, clock, and adder).
 *				The program will fetch an instruction, translate it, perform a read or write, and then repeat.
 *
 *	Modules:
 *				testBench	- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *				Clock		- a module that changes the output signal every ten ticks
 *				PC			- a module that adjusts the current address to the next address during every positive clock edge
 *				IM			- a module that reads and translates the current address  to read data from memory and/or write data to memory
 *				Adder		- a module which calculates and outputs the next address by adding four to the current address
 */

 
/*------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	testBench		- a module for testing whether the program functions by connecting all of the modules and printing to the output
 *	Ports:	None
 */
module testBench;								// establish the testBench module
	wire clock;									// establish clock as a 1 bit wire
	wire [31:0] currentAddress, newAddress,		// establish currentAddress, newAddress, data1, and data2 as 32 bit wires
	 data1, data2;
	
	Clock c (clock);							// instantiate a Clock							
	PC pc (currentAddress, newAddress, clock);	// instantiate a PC
	IM im (data1, data2, currentAddress, clock);// instantiate an IM
	Adder adder (newAddress, currentAddress);	// instantiate an Adder
	
	initial										// initially:
	begin
												// prints the following output whenever the mentioned variables are altered (for testing)
		$monitor ("clock=%d, current=%d, new=%d, data1=%d, data2=%d", clock, currentAddress, newAddress, data1, data2);
		#1000 $finish;							// 		as a failsafe, end the program after 1000 ticks
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Clock			- a module that changes the output signal every ten ticks
 *	Ports:
 *			clock			- a 1 bit output wire which toggles between 1 and 0
 */
module Clock(output clock);						// establish the Clock module
	reg clock;									// establish clock as a reg
	
	initial										// initially:
		clock = 0;								// 		set clock to 0
		
	always										// forever:
		#10 clock = ~clock;						// 		toggle the value of clock after 10 clock ticks
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	PC				- a module that adjusts the current address to the next address during every positive clock edge
 *	Ports:
 *			currentAddress	- a 32 bit output wire representing the address that the system is currently on
 *			newAddress		- a 32 bit input wire representing the next address that needs to be read
 *			clock			- a 1 bit input wire which toggles between 1 and 0
 */
module PC (output [31:0] currentAddress,		// establish the PC module
 input [31:0] newAddress, input clock);
	reg currentAddress;							// establish currentAddress as a reg
	
	initial										// initially:
		currentAddress = 32'habcdef00;			// 		set currentAddress to the first address
	
	always @ (posedge clock)					// during every positive clock tick:
		currentAddress = newAddress;			// 		set currentAddress to newAddress
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	IM				- a module that uses the current address to read or write data from memory
 *	Ports:
 *			data1			- a 32 bit output wire representing the data being read from the first register
 *			data2			- a 32 bit output wire representing the data being read from the second register
 *			currentAddress	- a 32 bit input wire representing the address that the system is currently on
 *			clock			- a 1 bit input wire which toggles between 1 and 0
 */
module IM (output [31:0] data1, data2,			// establish the IM module
 input [31:0] currentAddress, input clock);			
	wire [5:0] rs, rt, rd;						// establish rs, rt, and rd as six bit wires
	wire writeFlag;								// establish writeFlag as a one bit wire		
	wire [29:0] writeData;						// establish writeData as a thirty bit wire

	assign rs = currentAddress [7:2];			// continuously assign rs to the first 6 bits of currentAddress (excluding the first 2)
	assign rt = currentAddress [12:8];			// continuously assign rt to the next 6 bits of currentAddress after rs
	assign rd = currentAddress [17:13];			// continuously assign rd to the next 6 bits of currentAddress after rt
	assign writeFlag = currentAddress [18:18];	// continuously assign writeFlag to the next 1 bit of currentAddress after rd
	assign writeData = currentAddress [31:2];	// continuously assign writeData to currentAddress (excluding the first 2 bits)
	
	reg [31:0] memory [31:0];					// establish memory as 32 registers, each 32 bits in length
	
	assign data1 = memory [rs];					// continuously assign data1 to the rs register of memory
	assign data2 = memory [rt];					// continuously assign data2 to the rt register of memory
	
	always @ (posedge clock)					// during every positive clock tick:
	begin
		if (writeFlag)							// 		if the writeFlag is triggered in the address
			memory [rd] = writeData;			//			write the data from writeData into the memory at rd
	end
endmodule


/*------------------------------------------------------------------------------------------------------------------------------------------/
 *  Module:	Adder			- a module which calculates the next address by adding four to the current address
 *	Ports:
 *			newAddress		- a 32 bit output wire representing the next address that needs to be read
 *			currentAddress	- a 32 bit input wire representing the address that the system is currently on
 */
module Adder (output [31:0] newAddress,			// establish the Adder module
 input [31:0] currentAddress);
	assign newAddress = currentAddress + 4;		// continuously assign newAddress to be the currentAddress plus four (the next address)
endmodule