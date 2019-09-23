`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/04/2018 01:02:29 PM
// Module Name: stoch_div
// Description: 
//  Computes the stochastic quotient y = a / b
//////////////////////////////////////////////////////////////////////////////////
module stoch_div(CLK, nRST, a, b, y);

// params
parameter COUNTER_SIZE = 10;
parameter NEW_COUNTER_SIZE = (COUNTER_SIZE < 10) ? 10 : COUNTER_SIZE;

// I/O
input CLK, nRST;
input a, b;
output y;

// internal wires
reg signed [NEW_COUNTER_SIZE-1:0] counter, next_counter;
reg b_and;
wire next_b_and;
wire [1:0] a_x2, b_and_x2;
wire signed [NEW_COUNTER_SIZE-1:0] new_counter;
wire [63:0] r;

fibonacci_lfsr_64 lfsr(
    .CLK(CLK),
    .nRST(nRST),
    .r(r)
    );

assign a_x2 = a << 1;
assign b_and_x2 = b_and << 1;
assign new_counter = counter + a_x2 - b_and_x2;
assign y = (next_counter > $signed({{(NEW_COUNTER_SIZE-5){1'b0}}, r[5:0]})) ? 1'b1 : 1'b0;
assign next_b_and = y & b;

always @(posedge CLK) begin
    if (!nRST) begin
        counter <= {NEW_COUNTER_SIZE{1'b0}};
        b_and <= 1'b0;
    end
    else begin
        counter <= next_counter;
        b_and <= next_b_and;
    end
end

always @(new_counter) begin
    if (new_counter < -8'sd100) next_counter <= -8'sd100;
    else next_counter <= new_counter;
end

endmodule
