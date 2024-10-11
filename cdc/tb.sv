

module tb();

    logic clk0;
    logic clk1;

    logic rst0;
    logic rst1;

    logic req0;
    logic req1;

    logic ack0;
    logic ack1;

    logic [15:0] data0;
    logic [15:0] data1;

    CDC dut(
        .clk_l(clk0),
        .clk_r(clk1),
        .rst_l(rst0),
        .rst_r(rst1),
        .req_l(req0),
        .req_r(req1),
        .ack_l(ack0),
        .ack_r(ack1),
        .data_l(data0),
        .data_r(data1)
    );

    // Clock generation
    always #4 clk0 = ~clk0;
    always #10 clk1 = ~clk1;

    initial begin

        $dumpfile("waveform.vcd");
        $dumpvars;

    end

    initial begin

        clk0 = 1'b0;

        // Initialize signals
        rst0 = 1'b1;
        req0 = 1'b0;
        data0 = 16'd0;
        #8;

        // Deassert reset
        rst0 = 1'b0;
        #8;

        // Send first operand
        data0 = 16'h4444;
        req0 = 1'b1;
        wait (ack0 == 1'b1);

        #16;

        $finish;

    end

    initial begin 

        clk1 = 1'b0;

        // Initialize signals
        rst1 = 1'b1;
        req1 = 1'b0;
        data1 = 16'd0;
        #20;

        // Deassert reset
        rst1 = 1'b0;
        #20;

        // Send first operand
        wait (req1 == 1'b1);
        #10;

        // send ack
        ack1 = 1'b1;
        #10;

        $finish;
    end

endmodule