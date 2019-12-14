module formal (
    input wire       clk,

    input wire [7:0] key,
    input wire       mod,
    input wire       delete,
    input wire [7:0] val
);

    reg rst = 1;
    always @(posedge clk)
        rst <= 0;

    wire       valid;
    wire [7:0] res;
    wire       busy;

    cuckoo cuckoo_inst (
        .clk(clk),
        .rst(rst),

        .key(key),
        .mod(mod),
        .delete(delete),
        .val(val),

        .valid(valid),
        .res(res),
        .busy(busy)
    );

    //Formal properties

    always @(*)
        cover(!rst && !busy && valid && res == 123);

    (* anyconst *) reg [7:0] f_key;

    reg       f_valid = 0;
    reg [7:0] f_value;

    always @(posedge clk)
        if (!rst && !busy && mod && key == f_key)
            if (delete)
                f_valid <= 0;
            else begin
                f_valid <= 1;
                f_value <= val;
            end

    reg f_past_lookup = 0;
    always @(posedge clk)
        f_past_lookup <= !rst && !busy && !mod && key == f_key;

    always @(*)
        if (f_past_lookup) begin
            assert (f_valid == valid);
            if (f_valid)
                assert (f_value == res);
        end

endmodule

