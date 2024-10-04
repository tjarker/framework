
all: build/libmodel.so

build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a build/verilated_timing.o: GCD.sv sim.cpp
	verilator --cc --trace --timing --build --Mdir build -CFLAGS "-fPIC" GCD.sv sim.cpp

build/libmodel.so: build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a build/verilated_timing.o
	g++ -shared -o $@ $^ -pthread -lpthread -latomic
