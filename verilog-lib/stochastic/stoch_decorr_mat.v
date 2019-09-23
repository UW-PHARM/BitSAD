`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/04/2018 03:43:26 PM
// Module Name: stoch_decorr_mat
// Description: 
//  Instantiates matrix of stoch_decorr's.
//////////////////////////////////////////////////////////////////////////////////
module stoch_decorr_mat(CLK, nRST, A, Y);

// parameters
parameter COUNTER_SIZE = 8;
parameter STEP_VAL = 16;
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
            stoch_decorr #(.COUNTER_SIZE(COUNTER_SIZE), .STEP_VAL(STEP_VAL)) decorr(
                .CLK(CLK),
                .nRST(nRST),
                .a(A[(i*NUM_COLS)+j]),
                .y(Y[(i*NUM_COLS)+j])
                );
        end
    end 
endgenerate

endmodule
