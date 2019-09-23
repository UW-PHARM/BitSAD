`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/06/2018 10:53:28 AM
// Module Name: stoch_l2_norm
// Description: 
//  Computers the L2 norm of (up - un).
//////////////////////////////////////////////////////////////////////////////////
module stoch_l2_norm(CLK, nRST, up, un, y);

// parameters
parameter COUNTER_SIZE = 8;
parameter SQ_RT_COUNTER_SIZE = (COUNTER_SIZE + 1 < 10) ? 10 : COUNTER_SIZE + 1;
parameter STEP_VAL = 16;
parameter VEC_LEN = 2;

// I/O
input CLK, nRST;
input [VEC_LEN-1:0] up, un;
output y;

// internal wires
wire [VEC_LEN-1:0] u, u_decorr;
wire y_sq;

assign u = up | un;

stoch_decorr_mat #(.COUNTER_SIZE(COUNTER_SIZE), .STEP_VAL(STEP_VAL), .NUM_ROWS(VEC_LEN), .NUM_COLS(1)) decorr(
	.CLK(CLK),
	.nRST(nRST),
	.A(u),
	.Y(u_decorr)
	);

stoch_dot_prod #(.VEC_LEN(VEC_LEN)) dot_prod (
	.CLK(CLK),
	.nRST(nRST),
	.u(u),
	.v(u_decorr),
	.y(y_sq)
	);

stoch_square_root #(.COUNTER_SIZE(SQ_RT_COUNTER_SIZE)) sq_root(
	.CLK(CLK),
	.nRST(nRST),
	.a(y_sq),
	.y(y)
	);

endmodule
