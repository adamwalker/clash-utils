`default_nettype none
module formal (
    input wire        clk,

    input wire        valid,
    input wire [15:0] data_in
);

    reg rst = 1;
    always @(posedge clk)
        rst <= 0;

    wire [15:0] golden_data_out;
    wire [15:0] dut_data_out;

    (* anyconst *) wire [127:0] coeffs;

    golden_fir golden_fir_inst (
        .clk(clk),
        .rst(rst),

        .coeffs(coeffs),
        .valid(valid),
        .data_in(data_in),
        .data_out(golden_data_out)
    );

    transposed_fir transposed_fir_inst (
        .clk(clk),
        .rst(rst),

        .coeffs(coeffs),
        .valid(valid),
        .data_in(data_in),
        .data_out(dut_data_out)
    );

    //Formal properties
    reg vld_toggle = 0;
    always @(posedge clk)
        vld_toggle <= !vld_toggle;

    always @*
        //Ideally, valid would be allowed to drop out. But... the solver
        //just hangs forever if I uncomment the line below. 
        //if (vld_toggle)
            assume(valid);

    reg [15:0] golden_data_out_d = 0;
    always @(posedge clk)
        if (valid)
            golden_data_out_d <= golden_data_out;

    reg rst_d = 1;
    always @(posedge clk)
        if(valid)
            rst_d <= rst;

    reg valid_d = 0;
    always @(posedge clk)
        valid_d <= valid;

    always @* 
        if (!rst_d && valid_d)
            assert (golden_data_out_d == dut_data_out);

endmodule

