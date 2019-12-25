`default_nettype none
module formal (
);

    (* anyconst *) wire [7:0] binary_in;

    wire [7:0] gray;
    wire [7:0] gray_plus_1;
    wire [7:0] binary_out;

    binary_to_gray b2g_inst (
        .binary_in(binary_in),
        .gray_out(gray)
    );

    binary_to_gray b2g_inst2 (
        .binary_in(binary_in + 1),
        .gray_out(gray_plus_1)
    );

    gray_to_binary g2b_inst (
        .gray_in(gray),
        .binary_out(binary_out)
    );

    wire [7:0] diff = gray ^ gray_plus_1;

    //Formal properties
    always @* begin
        assert(binary_in == binary_out);
        assert(diff[0] + diff[1] + diff[2] + diff[3] + diff[4] + diff[5] + diff[6] + diff[7] == 1);
    end

endmodule

