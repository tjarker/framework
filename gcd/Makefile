
all: build/gcd_model.so

build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a: GCD.sv sim.cpp
	verilator --cc --trace --timing --build --Mdir build -CFLAGS "-fPIC -fpermissive" GCD.sv sim.cpp

build/gcd_model.so: build/libVGCD.a build/sim.o build/libverilated.a build/VGCD__ALL.a
	g++ -shared -o $@ $^ -pthread -lpthread -latomic
