package framework

import java.nio.file.Path
import java.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption

object MakefileGenerator {

  def generate(m: ModuleInterface, p: Path) = {
    Files.createDirectories(p)
    Files.write(
      p.resolve("Makefile"), 
      makefile(m).getBytes(StandardCharsets.UTF_8), 
      StandardOpenOption.CREATE, 
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }

  def makefile(m: ModuleInterface) = 
    s"""
       |all: build/lib${m.name}.so
       |
       |build/libV${m.name}.a build/sim.o build/libverilated.a build/V${m.name}__ALL.a: ${m.files.map(Path.of(_).toAbsolutePath()).mkString(" ")} sim.cpp
       |\tverilator --cc -j $$(shell nproc) --trace --build --Mdir build --top ${m.name} -CFLAGS "-fPIC -fpermissive" ${m.files.map(Path.of(_).toAbsolutePath()).mkString(" ")} sim.cpp
       |
       |build/lib${m.name}.so: build/libV${m.name}.a build/sim.o build/libverilated.a build/V${m.name}__ALL.a
       |\tg++ -shared -o $$@ build/libV${m.name}.a build/sim.o build/libverilated.a build/V${m.name}__ALL.a  -pthread -lpthread -latomic
       |
       |clean_copies:
       |\trm -rf build/lib${m.name}_*.so
       |""".stripMargin

}

object HarnessGenerator {

  def generate(m: ModuleInterface, p: Path) = {
    Files.createDirectories(p)
    Files.write(
      p.resolve("sim.cpp"), 
      harness(m).getBytes(StandardCharsets.UTF_8), 
      StandardOpenOption.CREATE, 
      StandardOpenOption.TRUNCATE_EXISTING
    )
  }


  def harness(m: ModuleInterface): String = {
    val name = m.name
    s"""
      |${includes(name)}
      |${SimContextClass(name)}
      |
      |${functionInterfaces(name)}
      |
      |${createAndDestroy(name)}
      |
      |${tick(name)}
      |
      |${setInput(m)}
      |${setInputWide(m)}
      |
      |${getOutput(m)}
      |${getOutputWide(m)}
      |
      |${getRegister(m)}
    """.stripMargin
  }

  def includes(name: String) = 
    s"""#include "V$name.h"
       |#include "V${name}___024root.h"
       |#include "verilated.h"
       |#include "verilated_vcd_c.h"
       |#include "stdint.h"""".stripMargin

  def SimContextClass(name: String) = 
    s"""class SimulationContext_$name {
       |  public:
       |  VerilatedContext* contextp;
       |  V$name* model;
       |  VerilatedVcdC* tfp;
       |
       |  SimulationContext_$name(const char* name, const char* wave_file, const char* time_resolution) {
       |    contextp = new VerilatedContext;;
       |    model = new V$name(contextp, name);;
       |    tfp = new VerilatedVcdC;;
       |
       |    contextp->traceEverOn(true);
       |    model->trace(tfp, 99);
       |    tfp->set_time_unit(time_resolution);
       |    tfp->set_time_resolution(time_resolution);
       |    tfp->open(wave_file);
       |  }
       |
       |  ~SimulationContext_$name() {
       |    tfp->flush();
       |    tfp->close();
       |    model->final();
       |
       |    delete model;
       |    delete tfp;
       |    delete contextp;
       |  }
       |};""".stripMargin

  def functionInterfaces(name: String) = 
    s"""extern "C" {
       |  SimulationContext_$name * createSimContext_$name(const char* name, const char* wave_file, const char* time_resolution);
       |  void destroySimContext_$name(SimulationContext_$name * id);
       |
       |  void tick_$name(SimulationContext_$name * ctx, uint32_t targetCycle);
       |
       |  void setInput_$name(SimulationContext_$name * ctx, uint64_t id, uint64_t val);
       |  uint64_t getOutput_$name(SimulationContext_$name * ctx, uint64_t id);
       |
       |  void setInputWide_$name(SimulationContext_$name * ctx, uint64_t id, uint32_t val[]);
       |  void getOutputWide_$name(SimulationContext_$name * ctx, uint64_t id, uint32_t val[]);
       |
       |  void getRegister_$name(SimulationContext_$name * ctx, uint64_t id, uint32_t val[]);
       |
       |  void quack_$name();
       |}""".stripMargin
  
  def createAndDestroy(name: String) = 
    s"""SimulationContext_$name * createSimContext_$name(const char* name, const char* wave_file, const char* time_resolution) {
       |  return new SimulationContext_$name(name, wave_file, time_resolution);
       |}
       |
       |void destroySimContext_$name(SimulationContext_$name * id) {
       |  delete id;
       |}
       |
       |void quack_$name() {
       |  printf("Model for $name says quack!\\n");
       |}""".stripMargin

  def tick(name: String) = 
    s"""void tick_$name(SimulationContext_$name * ctx, uint32_t targetCycle) {
       |  while (ctx->contextp->time() < targetCycle) {
       |    ctx->model->eval();
       |    ctx->tfp->dump(ctx->contextp->time());
       |    ctx->contextp->timeInc(1);
       |    ctx->tfp->flush();
       |  }
       |}""".stripMargin

  def setInput(m: ModuleInterface) = {
    val narrowInputs = m.inputs
      .filter(_.width.toInt <= 64)
      .map(i => i -> m.portToId(i))
      .map { case (in, id) =>
        s"case $id: ctx->model->${in.name} = val; break;"  
      }
    
    s"""void setInput_${m.name}(SimulationContext_${m.name} * ctx, uint64_t id, uint64_t val) {
       |  switch(id) {
       |    ${narrowInputs.mkString("\n    ")}
       |    default: break;
       |  }
       |}""".stripMargin
  }

  def setInputWide(m: ModuleInterface) = {
    val wideInputs = m.inputs
      .filter(_.width.toInt > 64)
      .map(i => i -> m.portToId(i))
      .map { case (in, id) =>
        val words = math.ceil(in.width.toInt / 32.0).toInt
        s"case $id: for (int i = 0; i < $words; i++) ctx->model->${in.name}.data()[i] = val[i]; break;"  
      }
    
    s"""void setInputWide_${m.name}(SimulationContext_${m.name} * ctx, uint64_t id, uint32_t val[]) {
       |  switch(id) {
       |    ${wideInputs.mkString("\n    ")}
       |    default: break;
       |  }
       |}""".stripMargin
  }

  def getOutput(m: ModuleInterface) = {
    val narrowOutputs = m.outputs
      .filter(_.width.toInt <= 64)
      .map(o => o -> m.portToId(o))
      .map { case (out, id) =>
        s"case $id: return ctx->model->${out.name}; break;"  
      }
    
    s"""uint64_t getOutput_${m.name}(SimulationContext_${m.name} * ctx, uint64_t id) {
       |  switch(id) {
       |    ${narrowOutputs.mkString("\n    ")}
       |    default: return 0;
       |  }
       |}""".stripMargin
  }

  def getOutputWide(m: ModuleInterface) = {
    val wideOutputs = m.outputs
      .filter(_.width.toInt > 64)
      .map(o => o -> m.portToId(o))
      .map { case (out, id) =>
        val words = math.ceil(out.width.toInt / 32.0).toInt
        s"case $id: for (int i = 0; i < $words; i++) val[i] = ctx->model->${out.name}.data()[i]; break;"  
      }
    
    s"""void getOutputWide_${m.name}(SimulationContext_${m.name} * ctx, uint64_t id, uint32_t val[]) {
       |  switch(id) {
       |    ${wideOutputs.mkString("\n    ")}
       |    default: break;
       |  }
       |}""".stripMargin
  }

  def regPathToVerilatorName(moduleName: String, p: String): String = {
    s"${moduleName}__DOT__${p.split("\\.").mkString("__DOT__")}"
  }

  def getRegister(m: ModuleInterface) = {
    val regMap = m.regs.map { r =>
      val path = regPathToVerilatorName(m.name, r.path)
      val words = math.ceil(r.w.toInt / 32.0).toInt
      if (words > 2) {
        s"""case ${m.regToId(r)}: for (int i = 0; i < $words; i++) val[i] = ctx->model->rootp->$path.data()[i]; break;"""
      } else {
        s"""case ${m.regToId(r)}: *((uint64_t *) val) = ctx->model->rootp->$path; break;"""
      }
    }
    val prints = m.regs.map { r =>
      val path = regPathToVerilatorName(m.name, r.path)
      s"""printf("Register ${r.path} = %d\\n", ctx->model->rootp->$path);"""
    }
    s"""void getRegister_${m.name}(SimulationContext_${m.name} * ctx, uint64_t id, uint32_t val[]) {
       |  
       |  switch(id) {
       |    ${regMap.mkString("\n    ")}
       |    default: break;
       |  }
       |}""".stripMargin
  }

}
