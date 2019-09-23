`timescale 1ns / 1ps
/////////////////////////////////////////////////////////////////////////////////////
// PHARM
// Carly Schulz
//
// fxp_mult
//		multiplication unit with fixed point support for the filter coefficients
/////////////////////////////////////////////////////////////////////////////////////
module fxp_mult(a, b, y);

// parameters
parameter BIT_WIDTH = 16;
parameter INT_WIDTH	= 1;

// I/O
input [(BIT_WIDTH - 1):0] a, b;
output signed [(BIT_WIDTH - 1):0] y;

// internal wires
wire [(BIT_WIDTH - 1):0] tmp;
wire sign_bit;

assign tmp = (a[(BIT_WIDTH - 2):0] * b[(BIT_WIDTH - 2):0]) >> (BIT_WIDTH - INT_WIDTH - 1);
assign sign_bit = a[BIT_WIDTH - 1] ^ b[BIT_WIDTH - 1];
assign y = sign_bit ? -tmp : tmp;

endmodule