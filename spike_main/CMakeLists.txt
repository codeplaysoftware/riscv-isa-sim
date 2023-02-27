# Copyright (C) Codeplay Software Limited. All Rights Reserved.

add_executable(spike
  spike.cc
)

add_executable(spike-dasm
  ${CMAKE_CURRENT_SOURCE_DIR}/../spike_dasm/spike-dasm.cc
)

target_link_libraries(spike riscv-sim fesvr dl)

target_link_libraries(spike-dasm riscv-sim fesvr dl)
