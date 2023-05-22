add_library(fesvr
  STATIC
  context.cc
  device.cc
  dtm.cc
  dummy.cc
  elf2hex.cc
  elfloader.cc
  htif.cc
  htif_hexwriter.cc
  htif_pthread.cc
  memif.cc
  option_parser.cc
  rfb.cc
  syscall.cc
  term.cc
  tsi.cc
)

target_include_directories(fesvr PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${SPIKE_SOURCE_DIR} # Other subprojects use <fesvr/foo.h> includes
)

target_include_directories(fesvr PRIVATE
    ${SPIKE_SOURCE_DIR}/riscv
    ${SPIKE_BINARY_DIR} # generated headers like config.h and icache.h
    ${SPIKE_SOURCE_DIR}/softfloat
)

target_compile_definitions(fesvr PRIVATE
    -DPREFIX="${CMAKE_INSTALL_PREFIX}"
)

target_link_libraries(fesvr PRIVATE pthread)
