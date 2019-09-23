`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/01/2018 04:40:30 PM
// Module Name: fibonacci_lfsr_64
// Description: 
//  Generates a pseudorandom 64-bit integer using a Fibonacci LFSR.
//  https://www.xilinx.com/support/documentation/application_notes/xapp052.pdf 
//////////////////////////////////////////////////////////////////////////////////
module fibonacci_lfsr_64(CLK, nRST, r);

// parameters
parameter SEED = 64'hFEEDBABEDEADBEEF;

// I/O
input CLK, nRST;
output reg [63:0] r;

// internal wires
reg [63:0] shift_reg, next_shift_reg;
reg [63:0] next_r;
wire shift_in;

assign shift_in = shift_reg[63] ^ shift_reg[62] ^ shift_reg[60] ^ shift_reg[59];

always @(posedge CLK) begin
    if (!nRST) begin
        shift_reg <= SEED;
        r <= 64'd0;
    end
    else begin
        shift_reg <= next_shift_reg;
        r <= next_r;
    end
end

always @(shift_reg) begin
    next_shift_reg <= {shift_reg[62:0], shift_in};
    next_r <= {r[62:0], shift_reg[63]};
end

endmodule
