`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 04/05/2018 07:35:15 PM
// Module Name: stoch_avg_mat
// Description: 
//  Instantiates matrix of stoch_avg.
//  Accepts inputs and outputs as row major vectors (indexed first by population).
//////////////////////////////////////////////////////////////////////////////////
module stoch_avg_mat(CLK, nRST, A, Y);

// parameters
parameter NUM_POPS = 2;
parameter NUM_ROWS = 3;
parameter NUM_COLS = 3;

// I/O
input CLK, nRST;
input [(NUM_ROWS*NUM_COLS*NUM_POPS)-1:0] A;
output [(NUM_ROWS*NUM_COLS)-1:0] Y;

// internal wires
reg [(NUM_ROWS*NUM_COLS*NUM_POPS)-1:0] A_pop_minor;

integer m, n, p;
always @(*) begin
	for (m = 0; m < NUM_ROWS; m = m + 1) begin
		for (n = 0; n < NUM_COLS; n = n + 1) begin
			for (p = 0; p < NUM_POPS; p = p + 1) begin
				A_pop_minor[m*(NUM_COLS*NUM_POPS)+(n*NUM_POPS)+p] <= A[p*(NUM_ROWS*NUM_COLS)+m*(NUM_COLS)+n];
			end
		end
	end
end

genvar i, j;
generate
    for (i = 0; i < NUM_ROWS; i = i + 1) begin : row
        for (j = 0; j < NUM_COLS; j = j + 1) begin: col
            stoch_avg #(
				.NUM_POPS(NUM_POPS)
			) avg (
				.CLK  (CLK),
				.nRST (nRST),
				.a(A_pop_minor[(i*NUM_COLS*NUM_POPS)+(j*NUM_POPS) +: NUM_POPS]),
                .y(Y[(i*NUM_COLS)+j])
			);
        end
    end 
endgenerate

endmodule