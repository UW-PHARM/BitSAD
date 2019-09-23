`timescale 1ns / 1ps
/////////////////////////////////////////////////////////////////////////////////////
// PHARM
// Carly Schulz
//
// determ_mult
//		multiplication unit for the deterministic bitstream 1 = 1 and 0 = -1
//       with FXP coefficient
/////////////////////////////////////////////////////////////////////////////////////
module determ_mult_fxp(a, b, y);

// parameters
parameter BIT_WIDTH = 16;

// I/O
input a;
input signed [(BIT_WIDTH - 1):0] b;
output signed [(BIT_WIDTH - 1):0] y;

assign y = a ? b : -b;

endmodule