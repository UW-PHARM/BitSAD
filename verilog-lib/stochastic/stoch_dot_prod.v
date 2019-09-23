`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/06/2018 08:48:40 AM
// Module Name: stoch_dot_prod
// Description: 
//  Computes the dot product of two vectors of stochastic bitstreams 
//////////////////////////////////////////////////////////////////////////////////
module stoch_dot_prod(CLK, nRST, u, v, y);

// parameters
parameter VEC_LEN = 2;
parameter COUNTER_SIZE = 8;

// I/O
input CLK, nRST;
input [VEC_LEN-1:0] u, v;
output y;

// internal wires
wire [VEC_LEN-1:0] c;
reg signed [COUNTER_SIZE-1:0] sum [0:VEC_LEN-2];
wire signed [COUNTER_SIZE-1:0] new_counter;
reg signed [COUNTER_SIZE-1:0] counter, next_counter;

assign c = u & v;

integer i;
always @(*) begin
    for (i = 0; i < VEC_LEN - 1; i = i + 1) begin
        if (i == 0) sum[i] <= c[i] + c[i + 1];
        else sum[i] <= sum[i - 1] + c[i + 1];
    end
end

assign new_counter = counter + sum[VEC_LEN - 2];
assign y = (new_counter >= $signed({{(COUNTER_SIZE - 1){1'b0}}, 1'b1})) ? 1'b1 : 1'b0;

always @(posedge CLK) begin
    if (!nRST) counter <= {COUNTER_SIZE{1'b0}};
    else counter <= next_counter;
end

always @(new_counter, y) begin
    next_counter <= new_counter - y;
end

endmodule