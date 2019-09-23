`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 03/06/2018 09:35:24 AM
// Module Name: stoch_matrix_mult
// Description: 
//  Instantiates a stochastic matrix multiplier (via stoch_dot_prod).
//  Accepts inputs and outputs as row major vectors.
//////////////////////////////////////////////////////////////////////////////////
module stoch_matrix_mult(CLK, nRST, A, B, Y);

// parameters
parameter NUM_ROWS = 2;
parameter NUM_MID = 2;
parameter NUM_COLS = 2;

// I/O
input CLK, nRST;
input [(NUM_ROWS*NUM_MID)-1:0] A;
input [(NUM_MID*NUM_COLS)-1:0] B;
output [(NUM_ROWS*NUM_COLS)-1:0] Y;

// internal wires
reg [(NUM_MID*NUM_COLS)-1:0] B_transpose;

always @(*) begin : transpose
    integer m, n;
    for (m = 0; m < NUM_MID; m = m + 1) begin
        for (n = 0; n < NUM_COLS; n = n + 1) begin
            B_transpose[(n*NUM_MID) + m] <= B[(m*NUM_COLS) + n];
        end
    end
end

genvar i, j;
generate
    for (i = 0; i < NUM_ROWS; i = i + 1) begin : row
        for (j = 0; j < NUM_COLS; j = j + 1) begin : col
            stoch_dot_prod #(.VEC_LEN(NUM_MID)) dot_prod(
                .CLK(CLK),
                .nRST(nRST),
                .u(A[(i*NUM_MID) +: NUM_MID]),
                .v(B_transpose[(j*NUM_MID) +: NUM_MID]),
                .y(Y[(i*NUM_COLS)+j])
                );
        end
    end 
endgenerate

endmodule