`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: PHARM
// Engineer: Kyle Daruwalla
// 
// Create Date: 02/28/2018 07:41:40 PM
// Module Name: stoch_monitor
// Description: 
//  Monitors a stochastic number.
//////////////////////////////////////////////////////////////////////////////////
module stoch_monitor(CLK, nRST, a, counter);

// params
parameter MAX_SIZE = 64;

// I/O
input CLK, nRST;
input a;
output reg [MAX_SIZE-1:0] counter;

always @(posedge CLK) begin
    if (!nRST) counter <= 64'b0;
    else if (a == 1'b1) counter <= counter + 64'b1;
    else counter <= counter;
end

endmodule