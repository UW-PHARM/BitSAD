`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 02/28/2018 08:37:06 PM
// Module Name: stoch_sat_sub
// Description: 
//  Performs max(a - b, 0) 
//////////////////////////////////////////////////////////////////////////////////
module stoch_sat_sub(CLK, nRST, a, b, y);

// parameters
parameter COUNTER_SIZE = 8;

// I/O
input CLK, nRST;
input a, b;
output y;

// internal wires
wire c;
wire inc, dec;
wire count_up;
wire thresh;
reg [COUNTER_SIZE-1:0] counter, next_counter;

assign c = a ^ b;
assign inc = c & b;
assign dec = c & a;
assign count_up = counter + inc;
assign thresh = (counter == {COUNTER_SIZE{1'b0}}) ? 1'b1 : 1'b0;

assign y = thresh & a & ~b;

always @(posedge CLK) begin
    if (!nRST) counter <= {COUNTER_SIZE{1'b0}};
    else counter <= next_counter;
end

always @(inc, dec) begin
    if (~|counter & ~inc & dec) next_counter <= {COUNTER_SIZE{1'b0}};
    else next_counter <= counter + inc - dec;
end

endmodule
