`default_nettype none
module formal (
    input wire        carry_in,
    input wire [31:0] x,
    input wire [31:0] y,
);

    wire [31:0] carrys;
    wire [31:0] sum;

    koggeStone cuckoo_inst (
        .carry_in(carry_in),
        .x(x),
        .y(y),
        .carrys(carrys),
        .sum(sum)
    );

    //Formal properties
    always @*
        assert(sum == x + y + carry_in);

endmodule

