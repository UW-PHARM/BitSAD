`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/01/2018 11:30:40 AM
// Module Name: stoch_add
// Description: 
//  Adds to stochastic bitstreams 
//////////////////////////////////////////////////////////////////////////////////
module stoch_add(CLK, nRST, a, b, y);

// parameters
parameter COUNTER_SIZE = 8;

// I/O
input CLK, nRST;
input a, b;
output y;

// internal wires
wire [COUNTER_SIZE-1:0] c;
reg [COUNTER_SIZE-1:0] counter, next_counter;

assign c = counter + a + b;
assign y = (c >= {{(COUNTER_SIZE - 1){1'b0}}, 1'b1}) ? 1'b1 : 1'b0;

always @(posedge CLK) begin
    if (!nRST) counter <= {COUNTER_SIZE{1'b0}};
    else counter <= next_counter;
end

always @(c, y) begin
    if (~|c & y) next_counter <= {COUNTER_SIZE{1'b0}};
    else next_counter <= c - y;
end

endmodule
