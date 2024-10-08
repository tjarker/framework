#include "VGCD.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support
#include <unordered_map>
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

typedef bool (*bool_cb)(void);

// arbitrary simulation event callback taking in all outputs and returning a boolean
typedef bool (*event_cb)(uint64_t[]);

const uint32_t NUM_OUTPUTS = 2;

uint64_t invocations = 0;

extern "C" {
    SimulationContext * createSimContext(const char* name, const char* wave_file, const char* time_resolution);
    void destroySimContext(SimulationContext * id);
    void setInput(SimulationContext * ctx, uint64_t id, uint64_t val);
    void tick(SimulationContext * ctx);
    uint64_t getOutput(SimulationContext * ctx, uint64_t id);
    void eval(bool_cb cb);
    uint64_t tickUntil(SimulationContext * ctx, event_cb cb);
}

void eval(bool_cb cb) {
    bool res = cb();
    printf("Eval result: %d\n", res);
}

uint64_t tickUntil(SimulationContext * ctx, event_cb cb) {
    uint64_t out_vals[NUM_OUTPUTS];

    uint64_t ticks = 0;
    
    bool res = false;
    do {
        tick(ctx);
        ticks++;
        for (int i = 0; i < NUM_OUTPUTS; i++) {
            out_vals[i] = getOutput(ctx, i);
        }
        printf("valid is %d\n", out_vals[0]);
        res = cb(out_vals);
        printf("res is %d\n", res);
    } while (!res);

    printf("Tick until ran %d cycles\n", ticks);

    invocations++;

    return ticks;
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
        case 0: ctx->GCD->a = val; break;
        case 1: ctx->GCD->b = val; break;
        case 2: ctx->GCD->loadValues = val; break;
    }
    invocations++;
}

uint64_t getOutput(SimulationContext * ctx, uint64_t id) {
    switch (id) {
        case 0: return ctx->GCD->isValid;
        case 1: return ctx->GCD->result;
    }
    invocations++;
}

void tick(SimulationContext * ctx) {
    ctx->GCD->clock = 0;
    ctx->GCD->eval();
    ctx->tfp->dump(ctx->contextp->time());
    ctx->contextp->timeInc(1);
    ctx->GCD->clock = 1;
    ctx->GCD->eval();
    ctx->tfp->dump(ctx->contextp->time());
    ctx->contextp->timeInc(1);
    invocations++;
}



/*



void sim_tick() {
    GCD->clock = 0;
    GCD->eval();
    tfp->dump(t);
    contextp->timeInc(1);
    GCD->clock = 1;
    GCD->eval();
    tfp->dump(t);
    contextp->timeInc(1);

    // TODO: we should use gcd->eventsPending() and gcd->nextTimeSlot() in the simulation loop
}


void set_tick_get(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned int ticks) {
    for (int i = 0; i < in_size; i++) {
        sim_set_input(in_ids[i], in_vals[i]);
    }
    sim_tick();

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        out_vals[i] = sim_get_output(i);
    }
    invocations++;
}

long set_tick_get_until_equal(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned long port_id, unsigned long value) {
    for (int i = 0; i < in_size; i++) {
        sim_set_input(in_ids[i], in_vals[i]);
    }

    unsigned long current_value = sim_get_output(port_id);
    long cycles;

    cycles = 0;
    do {
        sim_tick();
        cycles++;
        printf("current time is: %ld\n", contextp->time());
        current_value = sim_get_output(port_id);
    } while (current_value != value);

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        out_vals[i] = sim_get_output(i);
    }
    invocations++;

    return cycles;
}


*/