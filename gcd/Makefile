
all: build/libmodel.so

build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a: GCD.sv sim.cpp
	verilator --cc --trace --build --Mdir build -CFLAGS "-fPIC" GCD.sv sim.cpp

build/libmodel.so: build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a
	g++ -shared -o $@ $^
