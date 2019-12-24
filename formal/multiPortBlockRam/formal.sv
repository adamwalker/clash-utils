`default_nettype none
module formal (
    input wire   clk,

    input [7:0]  raddr0,
    input [7:0]  raddr1,

    input        wen0,
    input [7:0]  waddr0,
    input [31:0] wdata0,

    input        wen1,
    input [7:0]  waddr1,
    input [31:0] wdata1
);

    reg rst = 1;
    always @(posedge clk)
        rst <= 0;

    wire [31:0] rdata0;
    wire [31:0] rdata1;

    ram ram_inst (
        .clk(clk),
        .rst(rst),

        .raddr0(raddr0),
        .raddr1(raddr1),

        .wen0  (wen0),
        .waddr0(waddr0),
        .wdata0(wdata0),

        .wen1  (wen1),
        .waddr1(waddr1),
        .wdata1(wdata1),

        .rdata0(rdata0),
        .rdata1(rdata1)
    );

    //Formal properties

    (* anyconst *) reg [7:0] f_addr;
    reg [31:0] f_data = 0;
    reg [31:0] f_data_d = 0;

    always @*
        assume(!(wen0 && wen1 && waddr0 == waddr1));

    always @(posedge clk)
        if (!rst)
            if (wen0 && waddr0 == f_addr)
                f_data <= wdata0;
            else if (wen1 && waddr1 == f_addr)
                f_data <= wdata1;

    always @(posedge clk)
        f_data_d <= f_data;

    reg read_0 = 0;
    reg read_1 = 0;

    always @(posedge clk) begin
        read_0 <= raddr0 == f_addr;
        read_1 <= raddr1 == f_addr;
    end

    always @(*) begin
        if (read_0)
            assert(rdata0 == f_data_d);
        if (read_1) 
            assert(rdata1 == f_data_d);
    end

endmodule

