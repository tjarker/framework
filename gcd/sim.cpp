#include "VGCD.h"       // Generated Verilated model header
#include "verilated.h"       // Verilator functions
#include "verilated_vcd_c.h" // VCD tracing support

extern "C" {
    void sim_init();
    void sim_set(unsigned long id, unsigned long val);
    unsigned long sim_get(unsigned long id);
    void sim_tick();
    void sim_close();
}

VGCD* GCD;
VerilatedVcdC* tfp;
uint64_t t;

void sim_init() {
    GCD = new VGCD("GCD");
    tfp = new VerilatedVcdC;

    Verilated::traceEverOn(true);
    GCD->trace(tfp, 99);
    tfp->open("waveform.vcd");

    t = 0;
}

void sim_set(unsigned long id, unsigned long val) {
    switch (id) {
        case 0: GCD->a = val; break;
        case 1: GCD->b = val; break;
        case 3: GCD->loadValues = val; break;
    }
}

unsigned long sim_get(unsigned long id) {
    switch (id) {
        case 4: return GCD->isValid;
        case 5: return GCD->result;
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

void sim_close() {
    tfp->flush();
    tfp->close();
    GCD->final();
    delete GCD;
    delete tfp;
}
