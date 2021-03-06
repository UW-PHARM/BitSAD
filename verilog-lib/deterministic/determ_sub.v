`timescale 1ns / 1ps
/////////////////////////////////////////////////////////////////////////////////////
// PHARM
// Carly Schulz
//
// determ_sub
//		subtraction unit for the deterministic bitstream 1 = 1 and 0 = -1
/////////////////////////////////////////////////////////////////////////////////////
module determ_sub(a, b, y);

parameter BIT_WIDTH = 16;
parameter INT_WIDTH = 1;

// I/O
input a, b;
output signed [(BIT_WIDTH - 1):0] y;

// internal wires
wire signed [(BIT_WIDTH - 1):0] a_new, b_new;

assign a_new = a
               ? {{(INT_WIDTH - 1){1'b0}}, 2'sb01, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}}
               : {{(INT_WIDTH - 1){1'b1}}, 2'sb11, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}};
assign b_new = b
               ? {{(INT_WIDTH - 1){1'b0}}, 2'sb01, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}}
               : {{(INT_WIDTH - 1){1'b1}}, 2'sb11, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}};

assign y = a_new - b_new;

endmodule