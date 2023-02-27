# Copyright (C) Codeplay Software Limited. All Rights Reserved.

add_library(fdt
  STATIC
  fdt.c
  fdt_ro.c
  fdt_wip.c
  fdt_sw.c
  fdt_rw.c
  fdt_strerror.c
  fdt_empty_tree.c
  fdt_addresses.c
  fdt_overlay.c
)

target_include_directories(fdt PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)
