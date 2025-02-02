
# Requirements

- SBT
- JDK >= 19
- Verilator >= 5.002


# Use Case 1: Greatest Common Divisor

```sh
sbt "runMain GcdTest"
```

# Use Case 2: Clock Domain Crossing

```sh
sbt "runMain CdcTest"
```


# Use Case 3: Tiny ALU

```sh
sbt "runMain TinyAluTest"
```

# Use Case 4: Didactic SoC Subsystem

```sh
sbt "runMain DidacticSocTest"
```

# Waveforms

The waveforms for each use case are generated in the root directory of the project.