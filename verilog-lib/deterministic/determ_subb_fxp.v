`timescale 1ns / 1ps
/////////////////////////////////////////////////////////////////////////////////////
// PHARM
// Kyle Daruwalla
//
// determ_sub
//      subtraction for the deterministic bitstream 1 = 1 and 0 = -1 with FXP number
/////////////////////////////////////////////////////////////////////////////////////
module determ_subb_fxp(a, b, y);

// parameters
parameter BIT_WIDTH = 16;
parameter INT_WIDTH = 1;

// I/O
input signed [(BIT_WIDTH - 1):0] a;
input b;
output signed [(BIT_WIDTH - 1):0] y;

// internal wires
wire signed [(BIT_WIDTH - 1):0] b_new;

assign b_new = b
               ? {{(INT_WIDTH - 1){1'b0}}, 2'sb01, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}}
               : {{(INT_WIDTH - 1){1'b1}}, 2'sb11, {(BIT_WIDTH - INT_WIDTH - 1){1'b0}}};

assign y = a - b_new;

endmodule