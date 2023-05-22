FILE(GLOB SOFTLOAT_SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.c)

add_library(softfloat STATIC ${SOFTLOAT_SRC_FILES})

target_include_directories(softfloat PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_include_directories(softfloat PRIVATE
    ${SPIKE_BINARY_DIR} # generated headers like config.h and icache.h
)

