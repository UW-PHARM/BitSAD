`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/04/2018 03:16:40 AM
// Module Name: stoch_decorr
// Description: 
//  Decorrelates stochastic bitstream 
//////////////////////////////////////////////////////////////////////////////////
module stoch_decorr(CLK, nRST, a, y);

// parameters
parameter COUNTER_SIZE = 8;
parameter NEW_COUNTER_SIZE = (COUNTER_SIZE != 8) ? 8 : COUNTER_SIZE;
parameter STEP_VAL = 16;

// I/O
input CLK, nRST;
input a;
output y;

// internal wires
wire [NEW_COUNTER_SIZE-1:0] c;
reg [1:0] buffer, next_buffer;
reg [NEW_COUNTER_SIZE-1:0] counter, next_counter;
wire [63:0] r;
wire shift_in;
wire [NEW_COUNTER_SIZE-1:0] dec;

fibonacci_lfsr_64 lfsr(
    .CLK(CLK),
    .nRST(nRST),
    .r(r)
    );

assign c = (a == 1'b1) ? counter + STEP_VAL : counter;
assign shift_in = (r[NEW_COUNTER_SIZE-1:0] <= counter) ? 1'b1 : 1'b0;
assign y = buffer[1];
assign dec = (y == 1'b1) ? c - STEP_VAL : c;

always @(posedge CLK) begin
    if (!nRST) begin
        counter <= {NEW_COUNTER_SIZE{1'b0}};
        buffer <= 2'd0;
    end
    else begin
        counter <= next_counter;
        buffer <= next_buffer;
    end
end

always @(c, y) begin
    if ((c < STEP_VAL) & y) next_counter <= {NEW_COUNTER_SIZE{1'b0}};
    else next_counter <= dec;
end

always @(buffer, shift_in) begin
    next_buffer <= {buffer[0], shift_in};
end

endmodule