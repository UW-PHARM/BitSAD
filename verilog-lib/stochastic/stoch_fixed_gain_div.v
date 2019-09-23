`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/01/2018 03:07:51 PM
// Module Name: stoch_fixed_gain_div
// Description: 
//  Computes stochastic bitstream divided by fixed integer.
//////////////////////////////////////////////////////////////////////////////////
module stoch_fixed_gain_div(CLK, nRST, a, y);

// parameters
parameter COUNTER_SIZE = 8;
parameter GAIN = 2;
parameter ALPHA = (1 << COUNTER_SIZE) - 1;
parameter SCALED_GAIN = ALPHA * GAIN;

// I/O
input CLK, nRST;
input a;
output y;

// internal wires
wire [2*COUNTER_SIZE-1:0] c;
reg [2*COUNTER_SIZE-1:0] counter, next_counter;

assign c = (a == 1'b1) ? counter + ((1 << COUNTER_SIZE) - 1) : counter;
assign y = (c >= SCALED_GAIN) ? 1'b1 : 1'b0;

always @(posedge CLK) begin
    if (!nRST) counter <= {2*COUNTER_SIZE{1'b0}};
    else counter <= next_counter;
end

always @(c) begin
    if (y == 1'b1) next_counter <= c - SCALED_GAIN;
    else next_counter <= c;
end

endmodule
