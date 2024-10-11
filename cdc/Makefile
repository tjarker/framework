
all: build/libcdc.so

build/libVCDC.a build/sim.o build/libverilated.a build/VGCD__ALL.a: CDC.sv sim.cpp
	verilator --cc --trace --timing --build --Mdir build -CFLAGS "-fPIC" $^

build/libcdc.so: build/libVCDC.a build/sim.o build/libverilated.a build/VCDC__ALL.a
	g++ -shared -o $@ $^ -pthread -lpthread -latomic
