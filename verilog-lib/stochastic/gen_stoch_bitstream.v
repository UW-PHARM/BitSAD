`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 02/28/2018 07:41:40 PM
// Module Name: gen_stoch_bitstream
// Description: 
//  Generates a stochastic bitstream based on parameter MEAN.
//////////////////////////////////////////////////////////////////////////////////
module gen_stoch_bitstream(CLK, nRST, a);

// params
parameter SEED = 0;
parameter MEAN = (1 << 31);

// I/O
input CLK, nRST;
output reg a;

// internal wires
reg [31:0] r;
reg next_a;

always @(posedge CLK) begin
    if (!nRST) begin
        r <= $urandom(SEED);
        a <= 1'b0;
    end
    else begin
        r <= $urandom;
        a <= next_a;
    end
end

always @(r) begin
    if (r < MEAN) next_a <= 1'b1;
    else next_a <= 1'b0;
end

endmodule
