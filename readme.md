
# Requirements

- JDK >= 19
- Verilator >= 5.002


# Running Examples

```sh
sbt "runMain GcdSim" # a showcase of a simple testbench
sbt "runMain CdcSim" # a showcase of a testbench with multiple clocks
sbt "runMain TinyAluTest" # a showcase of a simple testbench with a model
sbt "runMain TinyAluUvm" # a UVM showcase
```