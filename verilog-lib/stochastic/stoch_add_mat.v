`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/01/2018 11:48:24 AM
// Module Name: stoch_add_mat
// Description: 
//  Instantiates a stochastic matrix adder.
//  Accepts inputs and outputs as row major vectors.
//////////////////////////////////////////////////////////////////////////////////
module stoch_add_mat(CLK, nRST, A, B, Y);

// parameters
parameter NUM_ROWS = 2;
parameter NUM_COLS = 2;

// I/O
input CLK, nRST;
input [(NUM_ROWS*NUM_COLS)-1:0] A, B;
output [(NUM_ROWS*NUM_COLS)-1:0] Y;

genvar i, j;
generate
    for (i = 0; i < NUM_ROWS; i = i + 1) begin : row
        for (j = 0; j < NUM_COLS; j = j + 1) begin: col
            stoch_add add(
                .CLK(CLK),
                .nRST(nRST),
                .a(A[(i*NUM_COLS)+j]),
                .b(B[(i*NUM_COLS)+j]),
                .y(Y[(i*NUM_COLS)+j])
                );
        end
    end 
endgenerate

endmodule
