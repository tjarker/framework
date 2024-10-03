#include "VGCD.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support

extern "C" {
    void sim_init();
    void sim_close();
    void set_tick_get(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned int ticks);
    void set_tick_get_until_equal(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned long port_id, unsigned long value);
}

VGCD* GCD;
VerilatedVcdC* tfp;
uint64_t t;

const long NUM_OUTPUTS = 2;

unsigned long invocations = 0;

void sim_init() {
    GCD = new VGCD("GCD");
    tfp = new VerilatedVcdC;

    Verilated::traceEverOn(true);
    GCD->trace(tfp, 99);
    tfp->open("waveform.vcd");

    t = 0;
    invocations = 1;
}

void sim_set(unsigned long id, unsigned long val) {
    switch (id) {
        case 0: GCD->a = val; break;
        case 1: GCD->b = val; break;
        case 2: GCD->loadValues = val; break;
    }
}

unsigned long sim_get(unsigned long id) {
    switch (id) {
        case 0: return GCD->isValid;
        case 1: return GCD->result;
    }
}

void sim_tick() {
    GCD->clock = 0;
    GCD->eval();
    tfp->dump(t);
    t++;
    Verilated::timeInc(1);
    GCD->clock = 1;
    GCD->eval();
    tfp->dump(t);
    t++;
    Verilated::timeInc(1);
}


void set_tick_get(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned int ticks) {
    for (int i = 0; i < in_size; i++) {
        sim_set(in_ids[i], in_vals[i]);
    }
    sim_tick();

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        out_vals[i] = sim_get(i);
    }
    invocations++;
}

void set_tick_get_until_equal(unsigned long in_ids[], unsigned long in_vals[], unsigned int in_size, unsigned long out_vals[], unsigned long port_id, unsigned long value) {
    for (int i = 0; i < in_size; i++) {
        sim_set(in_ids[i], in_vals[i]);
    }

    unsigned long current_value = sim_get(port_id);

    do {
        sim_tick();
        current_value = sim_get(port_id);
    } while (current_value != value);

    for (int i = 0; i < NUM_OUTPUTS; i++) {
        out_vals[i] = sim_get(i);
    }
    invocations++;
}

void sim_close() {

    printf("Native invocations: %lu\n", invocations);

    tfp->flush();
    tfp->close();
    GCD->final();
    delete GCD;
    delete tfp;
}
