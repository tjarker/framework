

module CDC #(
    parameter int W = 16
)(
    input  logic clk_l,
    input  logic clk_r,
    input  logic rst_l,
    input  logic rst_r,

    input  logic req_l,
    output logic req_r,

    output logic ack_l,
    input  logic ack_r,

    input  logic [W - 1:0] data_l,
    output logic [W - 1:0] data_r
);

    logic [1:0] req_sync;
    logic [1:0] ack_sync;

    assign req_r = req_sync[1];
    always_ff @(posedge clk_r) begin
        if (rst_r) req_sync <= 2'b00;
        else req_sync <= {req_sync[0], req_l};
    end

    assign ack_l = ack_sync[1];
    always_ff @(posedge clk_l) begin
        if (rst_l) ack_sync <= 2'b00;
        else ack_sync <= {ack_sync[0], ack_r};
    end

    assign data_r = data_l;


endmodule