`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
//
// Create Date: 09/23/2019
// Module Name: stoch_cross_prod
// Description:
//  Instantiates a stochastic cross product of 3D vectors
//////////////////////////////////////////////////////////////////////////////////
module stoch_cross_prod(CLK, nRST, u, v, y_p, y_m);

// parameters
parameter VEC_LEN = 3;

// I/O
input CLK, nRST;
input [VEC_LEN-1:0] u;
input [VEC_LEN-1:0] v;
output [VEC_LEN-1:0] y_p, y_m;

// internal wires
wire [VEC_LEN-1:0] mult1, mult2;

// hardcoded for 3D vectors only //

// multiples
assign mult1[0] = u[1] & v[2];
assign mult2[0] = u[2] & v[1];
assign mult1[1] = u[0] & v[2];
assign mult2[1] = u[2] & v[0];
assign mult1[2] = u[0] & v[1];
assign mult2[2] = u[1] & v[0];

// subtractions
stoch_sat_sub sub1p(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult1[0]),
    .b(mult2[0]),
    .y(y_p[0])
    );
stoch_sat_sub sub1m(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult2[0]),
    .b(mult1[0]),
    .y(y_m[0])
    );
stoch_sat_sub sub2p(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult1[1]),
    .b(mult2[1]),
    .y(y_p[1])
    );
stoch_sat_sub sub2m(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult2[1]),
    .b(mult1[1]),
    .y(y_m[1])
    );
stoch_sat_sub sub3p(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult1[2]),
    .b(mult2[2]),
    .y(y_p[2])
    );
stoch_sat_sub sub3m(
    .CLK(CLK),
    .nRST(nRST),
    .a(mult2[2]),
    .b(mult1[2]),
    .y(y_m[2])
    );

endmodule