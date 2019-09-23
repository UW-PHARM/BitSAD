`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/04/2018 01:30:29 PM
// Module Name: stoch_div_mat
// Description: 
//  Computes the element-wise stochastic quotient Y = A ./ B
//////////////////////////////////////////////////////////////////////////////////
module stoch_div_mat(CLK, nRST, A, B, Y);

// parameters
parameter COUNTER_SIZE = 10;
parameter DIV_COUNTER_SIZE = (COUNTER_SIZE + 1 < 10) ? 10 : COUNTER_SIZE + 1;
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
            stoch_div #(.COUNTER_SIZE(DIV_COUNTER_SIZE)) div(
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