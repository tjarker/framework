
#include "VGCD.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support
#include "stdint.h"


class SimulationContext {
    public:
    VerilatedContext* contextp;
    VGCD* GCD;
    VerilatedVcdC* tfp;

    SimulationContext(const char* name, const char* wave_file, const char* time_resolution) {
        contextp = new VerilatedContext;;
        GCD = new VGCD(contextp, name);;
        tfp = new VerilatedVcdC;;

        contextp->traceEverOn(true);
        GCD->trace(tfp, 99);
        tfp->set_time_unit("1ns");
        tfp->set_time_resolution(time_resolution);
        tfp->open(wave_file);
    }

    ~SimulationContext() {
        tfp->flush();
        tfp->close();
        GCD->final();

        delete GCD;
        delete tfp;
        delete contextp;
    }
};


extern "C" {
    SimulationContext * createSimContext(const char* name, const char* wave_file, const char* time_resolution);
    void destroySimContext(SimulationContext * id);
    void setInput(SimulationContext * ctx, uint64_t id, uint64_t val);
    void setInputWide(SimulationContext * ctx, uint64_t id, uint64_t val[]);
    void tick(SimulationContext * ctx, uint32_t targetCycle);
    uint64_t getOutput(SimulationContext * ctx, uint64_t id);
    void getOutputWide(SimulationContext * ctx, uint64_t id, uint64_t val[]);
}




SimulationContext * createSimContext(const char* name, const char* wave_file, const char* time_resolution) {
    SimulationContext * ctx = new SimulationContext(name, wave_file, time_resolution);
    return ctx;
}

void destroySimContext(SimulationContext * ctx) {
    delete ctx;
}

void tick(SimulationContext * ctx, uint32_t targetCycle) {
    while (ctx->contextp->time() < targetCycle) {
        ctx->GCD->eval();
        ctx->tfp->dump(ctx->contextp->time());
        ctx->contextp->timeInc(1);
        ctx->tfp->flush();
    }
}


void setInput(SimulationContext * ctx, uint64_t id, uint64_t val) {
    switch (id) {
        case 0: ctx->GCD->clock = val; break;
        case 1: ctx->GCD->reset = val; break;
        case 2: ctx->GCD->req = val; break;
    }
    ctx->GCD->eval();
}

void setInputWide(SimulationContext * ctx, uint64_t id, uint64_t val[]) {
    switch(id) {
        case 4: 
            for (int i = 0; i < 4; i++) ctx->GCD->loadVal.data()[i] = val[i];
            break;
    }
}

uint64_t getOutput(SimulationContext * ctx, uint64_t id) {
    switch (id) {
        case 3: return ctx->GCD->ack;
    }
}

void getOutputWide(SimulationContext * ctx, uint64_t id, uint64_t val[]) {
    switch(id) {
        case 5:
            for (int i = 0; i < 4; i++) val[i] = ctx->GCD->result.data()[i];
            break;
    }
}


