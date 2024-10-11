

module tb();

logic clock;
logic reset;
logic req;
logic ack;
logic [15:0] loadVal;
logic [15:0] result;

GCD dut(
    .clock(clock),
    .reset(reset),
    .req(req),
    .ack(ack),
    .loadVal(loadVal),
    .result(result)
);

// Clock generation
always begin
    #5 clock = ~clock;
end

initial begin

    $dumpfile("waveform.vcd");
    $dumpvars;

    clock = 1'b0;

    // Initialize signals
    reset = 1'b1;
    req = 1'b0;
    loadVal = 16'd0;
    #10;
    
    // Deassert reset
    reset = 1'b0;
    #10;
    
    // Send first operand
    loadVal = 16'h4444;
    req = 1'b1;
    wait (ack == 1'b1);
    #5;
    req = 1'b0;
    wait (ack == 1'b0);
    #5;
    
    // Send second operand
    loadVal = 16'h700C;
    req = 1'b1;
    wait (ack == 1'b1);
    #5;
    req = 1'b0;
    wait (ack == 1'b0);
    #5;
    
    // Wait for result
    #100;

    
    // Finish simulation
    $finish;

end

endmodule