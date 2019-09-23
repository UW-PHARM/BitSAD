`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/06/2018 02:48:06 PM
// Module Name: stoch_fixed_gain_div_mat
// Description: 
// 	Instantiates stoch_fixed_gain_div for matrices.
//  Accepts inputs and outputs as row major vectors.
//////////////////////////////////////////////////////////////////////////////////
module stoch_fixed_gain_div_mat(CLK, nRST, A, Y);

// parameters
parameter COUNTER_SIZE = 8;
parameter GAIN = 2;
parameter NUM_ROWS = 2;
parameter NUM_COLS = 2;

// I/O
input CLK, nRST;
input [(NUM_ROWS*NUM_COLS)-1:0] A;
output [(NUM_ROWS*NUM_COLS)-1:0] Y;

genvar i, j;
generate
    for (i = 0; i < NUM_ROWS; i = i + 1) begin : row
        for (j = 0; j < NUM_COLS; j = j + 1) begin: col
            stoch_fixed_gain_div #(.COUNTER_SIZE(COUNTER_SIZE), .GAIN(GAIN)) div(
                .CLK(CLK),
                .nRST(nRST),
                .a(A[(i*NUM_COLS)+j]),
                .y(Y[(i*NUM_COLS)+j])
                );
        end
    end 
endgenerate

endmodule
