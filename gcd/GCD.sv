`timescale 1ns / 1ns

module GCD(
    input signed [62:0] a,
    input signed [62:0] b,
    input clock,
    input loadValues,
    output logic isValid,
    output [62:0] result);

    reg signed [62:0] x;
    reg signed [62:0] y;
    wire isValid_internal;
    always @(posedge clock) begin
        if (loadValues) begin
            if (a > 0)
                x <= a;
            else
                x <= -a;

            if (b > 0)
                y <= b;
            else
                y <= -b;
        end
        else if (x > y)
            x <= x - y;
        else
            y <= y - x;
    end
    assign result = x;
    assign isValid_internal = y == 'h0;

    always @* begin
        isValid = #1 isValid_internal;
    end

    always @loadValues begin
        if (loadValues) begin
            $display("Calculating GCD of %X and %X.", a, b);
        end
    end
    always @isValid_internal begin
        if (isValid_internal) begin
            $display("Calculated GCD to be %X.", x);
        end
    end
endmodule