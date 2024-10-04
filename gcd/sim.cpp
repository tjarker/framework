#include "VGCD.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support
#include <unordered_map>
#include "stdint.h"

extern "C" {
    uint32_t createSimContext(const char* name, const char* wave_file, const char* time_resolution);
    void destroySimContext(uint32_t id);
    void setInput(uint32_t ctx, uint32_t id, uint64_t val);
    void tick(uint32_t ctx);
    uint64_t getOutput(uint32_t ctx, uint32_t id);
}



// Notes
// contextp->time() gives current time
void tick(uint32_t ctx);

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

uint32_t contextCounter = 0;
std::unordered_map<uint32_t, SimulationContext*> simulation_contexts;



const uint32_t NUM_OUTPUTS = 2;

uint64_t invocations = 0;

uint32_t createSimContext(const char* name, const char* wave_file, const char* time_resolution) {

    uint32_t id = contextCounter++;
    simulation_contexts[id] = new SimulationContext(name, wave_file, time_resolution);

    invocations = 1;

    return id;
}

void destroySimContext(uint32_t id) {

    printf("Native invocations: %lu\n", invocations);

    delete simulation_contexts[id];
    simulation_contexts.erase(id);
    
}


void setInput(uint32_t ctx, uint32_t id, uint64_t val) {
    SimulationContext* sim = simulation_contexts[ctx];
    switch (id) {
        case 0: sim->GCD->a = val; break;
        case 1: sim->GCD->b = val; break;
        case 2: sim->GCD->loadValues = val; break;
    }
    invocations++;
}

uint64_t getOutput(uint32_t ctx, uint32_t id) {
    SimulationContext* sim = simulation_contexts[ctx];
    switch (id) {
        case 0: return sim->GCD->isValid;
        case 1: return sim->GCD->result;
    }
    invocations++;
}

uint64_t get_output(uint32_t ctx, uint32_t id);

void tick(uint32_t ctx) {
    SimulationContext* sim = simulation_contexts[ctx];
    sim->GCD->clock = 0;
    sim->GCD->eval();
    sim->tfp->dump(sim->contextp->time());
    sim->contextp->timeInc(1);
    sim->GCD->clock = 1;
    sim->GCD->eval();
    sim->tfp->dump(sim->contextp->time());
    sim->contextp->timeInc(1);
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