#include "VCDC.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support
#include <unordered_map>
#include "stdint.h"




class SimulationContext {
    public:
    VerilatedContext* contextp;
    VCDC* model;
    VerilatedVcdC* tfp;

    SimulationContext(const char* name, const char* wave_file, const char* time_resolution) {
        contextp = new VerilatedContext;;
        model = new VCDC(contextp, name);;
        tfp = new VerilatedVcdC;;

        contextp->traceEverOn(true);
        model->trace(tfp, 99);
        tfp->set_time_unit("1ns");
        tfp->set_time_resolution(time_resolution);
        tfp->open(wave_file);
    }

    ~SimulationContext() {
        tfp->flush();
        tfp->close();
        model->final();

        delete model;
        delete tfp;
        delete contextp;
    }
};

typedef bool (*bool_cb)(void);

// arbitrary simulation event callback taking in all outputs and returning a boolean
typedef bool (*event_cb)(uint64_t[]);

const uint32_t NUM_OUTPUTS = 3;

uint64_t invocations = 0;

extern "C" {
    SimulationContext * createSimContext(const char* name, const char* wave_file, const char* time_resolution);
    void destroySimContext(SimulationContext * id);
    void setInput(SimulationContext * ctx, uint64_t id, uint64_t val);
    uint64_t getOutput(SimulationContext * ctx, uint64_t id);
    void tick(SimulationContext * ctx);
}



SimulationContext * createSimContext(const char* name, const char* wave_file, const char* time_resolution) {

    SimulationContext * ctx = new SimulationContext(name, wave_file, time_resolution);

    invocations = 1;

    return ctx;
}

void destroySimContext(SimulationContext * ctx) {

    printf("Native invocations: %lu\n", invocations);

    delete ctx;
    
}


void setInput(SimulationContext * ctx, uint64_t id, uint64_t val) {
    switch (id) {
        case 0: ctx->model->clk_l = val; break;
        case 1: ctx->model->clk_r = val; break;
        case 2: ctx->model->rst_l = val; break;
        case 3: ctx->model->rst_r = val; break;
        case 4: ctx->model->req_l = val; break;
        case 5: ctx->model->ack_r = val; break;
        case 6: ctx->model->data_l = val; break;
    }
    invocations++;
}

uint64_t getOutput(SimulationContext * ctx, uint64_t id) {
    switch (id) {
        case 0: return ctx->model->req_r;
        case 1: return ctx->model->ack_l;
        case 2: return ctx->model->data_r;
    }
    invocations++;
}

void tick(SimulationContext * ctx) {
    ctx->model->eval();
    ctx->tfp->dump(ctx->contextp->time());
    ctx->contextp->timeInc(1);
    invocations++;
}