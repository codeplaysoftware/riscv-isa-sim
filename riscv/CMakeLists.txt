# Compile all instructions.
set(INSN_DIR ${CMAKE_CURRENT_BINARY_DIR}/insn)
set(INSN_SOURCE_FILE ${INSN_DIR}/insns.cc)
set(INSN_HEADER_FILE ${SPIKE_BINARY_DIR}/insn_list.h)
set(INSN_TEMPLATE ${CMAKE_CURRENT_SOURCE_DIR}/insn_template.cc)
set(INSN_ENCODING ${CMAKE_CURRENT_SOURCE_DIR}/encoding.h)
set(INSN_GEN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/generate_insn.py)
file(MAKE_DIRECTORY ${INSN_DIR})

add_custom_command(OUTPUT ${INSN_SOURCE_FILE} ${INSN_HEADER_FILE}
                   COMMAND /usr/bin/python3 "${INSN_GEN_SCRIPT}" "${CMAKE_CURRENT_SOURCE_DIR}" "${INSN_SOURCE_FILE}" "${INSN_HEADER_FILE}"
                   DEPENDS ${INSN_GEN_SCRIPT} ${INSN_TEMPLATE} ${INSN_ENCODING})

add_library(riscv-sim
  STATIC
  cachesim.cc
  clint.cc
  csrs.cc
  debug_module.cc
  devices.cc
  dts.cc
  execute.cc
  extension.cc
  extensions.cc
  interactive.cc
  isa_parser.cc
  jtag_dtm.cc
  mmu.cc
  processor.cc
  profiler.cc
  ${CMAKE_CURRENT_SOURCE_DIR}/../disasm/regnames.cc
  remote_bitbang.cc
  rocc.cc
  rom.cc
  sim.cc
  triggers.cc
  ${CMAKE_CURRENT_SOURCE_DIR}/../disasm/disasm.cc
  ${INSN_SOURCE_FILE}
)

# Generate icache.h
set(ICACHE_HEADER_FILE ${SPIKE_BINARY_DIR}/icache.h)
set(MMU_HEADER_FILE ${CMAKE_CURRENT_SOURCE_DIR}/mmu.h)
set(ICACHE_GEN_SCRIPT ${CMAKE_CURRENT_SOURCE_DIR}/generate_icache.py)
add_custom_command(OUTPUT ${ICACHE_HEADER_FILE}
                   COMMAND /usr/bin/python3 "${ICACHE_GEN_SCRIPT}" "${MMU_HEADER_FILE}" "${ICACHE_HEADER_FILE}"
                   DEPENDS ${ICACHE_GEN_SCRIPT} ${MMU_HEADER_FILE})
add_custom_target(riscv-headers ALL DEPENDS ${ICACHE_HEADER_FILE})
add_dependencies(riscv-sim riscv-headers)

target_include_directories(riscv-sim PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${SPIKE_BINARY_DIR} # generated headers like config.h and icache.h
)

target_link_libraries(riscv-sim fesvr softfloat fdt)
