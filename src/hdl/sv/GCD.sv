module GCD #(
    parameter int W = 128
)(
    input  logic clock,
    input  logic reset,
    input  logic req,
    output logic ack,
    input  logic [W - 1:0] loadVal,
    output logic [W - 1:0] result
);

    typedef enum logic [2:0] {
        wait_a,
        ack_a,
        wait_b,
        compare,
        update_a,
        update_b,
        ack_result
    } state_t;

    state_t state;

    logic [W - 1:0] a;
    logic [W - 1:0] b;

    assign ack = (state == ack_a || state == ack_result);
    assign result = a;

    always_ff @(posedge clock) begin

        if (reset) begin
            state <= wait_a;
            a <= 0;
            b <= 0;
        end else begin
            case (state)
                wait_a: begin
                    a <= loadVal;
                    if (req) begin
                        $display("a = %d", loadVal);
                        state <= ack_a;
                    end
                end
                ack_a: begin
                    if (!req) state <= wait_b;
                end
                wait_b: begin
                    b <= loadVal;
                    if (req) begin
                        $display("b = %d", loadVal);
                        state <= compare;
                    end
                end
                compare: begin
                    if (a > b) state <= update_a;
                    else if (a < b) state <= update_b;
                    else state <= ack_result;
                end
                update_a: begin
                    a <= a - b;
                    state <= compare;
                end
                update_b: begin
                    b <= b - a;
                    state <= compare;
                end
                ack_result: begin
                    if (!req) state <= wait_a;
                end
                default: $display("Invalid state\n");
            endcase
        end
    end

endmodule