`timescale 1ns / 1ps
/////////////////////////////////////////////////////////////////////////////////////
// PHARM
// Carly Schulz
//
// fxp_sub
//		subtraction unit with fixed point support for the filter coefficients
/////////////////////////////////////////////////////////////////////////////////////
module fxp_sub(a, b, y);

// parameters
parameter BIT_WIDTH = 16;

// I/O
input signed [(BIT_WIDTH - 1):0] a, b;
output signed [(BIT_WIDTH - 1):0] y;

assign y = a - b;

endmodule