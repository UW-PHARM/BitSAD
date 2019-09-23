`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 04/05/2018 06:14:27 PM
// Module Name: stoch_avg
// Description: 
//  Averages several stochastic bitstreams.
//////////////////////////////////////////////////////////////////////////////////
module stoch_avg(CLK, nRST, a, y);

// parameters
parameter NUM_POPS = 2;
parameter COUNTER_SIZE = $clog2(2*NUM_POPS + 1);

// I/O
input CLK;
input nRST;
input [NUM_POPS-1:0] a;
output y;

// internal wires
reg [COUNTER_SIZE-2:0] sum [0:NUM_POPS-2];
wire [COUNTER_SIZE-1:0] new_counter;
reg [COUNTER_SIZE-1:0] counter, next_counter;

integer i;
always @(*) begin
    for (i = 0; i < NUM_POPS - 1; i = i + 1) begin
        if (i == 0) sum[i] <= a[i] + a[i + 1];
        else sum[i] <= sum[i - 1] + a[i + 1];
    end
end

assign new_counter = counter + sum[NUM_POPS - 2];
assign y = (new_counter >= $signed(NUM_POPS)) ? 1'b1 : 1'b0;

always @(posedge CLK) begin
    if (!nRST) counter <= {COUNTER_SIZE{1'b0}};
    else counter <= next_counter;
end

always @(new_counter, y) begin
	if (y == 1'b1) next_counter <= new_counter - NUM_POPS;
    else next_counter <= new_counter;
end

endmodule
