// See LICENSE for license details.

#ifndef _RISCV_PROFILER_H
#define _RISCV_PROFILER_H

#include "decode.h"
#include "encoding.h"

#include <memory>
#include <unordered_map>

class processor_t;

class profiler_abs_t {
public:
  virtual ~profiler_abs_t() = default;
  virtual void operator()(processor_t &, unsigned int) = 0;
};

class profiler_memory_op_t : public profiler_abs_t {
public:
  profiler_memory_op_t(reg_t tc, reg_t lc)
      : total_counter(tc), length_counter(lc) {}
  void operator()(processor_t &, unsigned int) override;

private:
  reg_t total_counter;
  reg_t length_counter;
};

class profiler_logic_op_t : public profiler_abs_t {
public:
  explicit profiler_logic_op_t(reg_t tc) : total_counter(tc) {}
  void operator()(processor_t &, unsigned int) override;

private:
  reg_t total_counter;
};

class profiler_atomic_op_t : public profiler_abs_t {
public:
  explicit profiler_atomic_op_t(reg_t rtc, reg_t rlc, reg_t ltc, reg_t wtc,
                                reg_t wlc)
      : read_total_counter(rtc), read_length_counter(rlc),
        logic_total_counter(ltc), write_total_counter(wtc),
        write_length_counter(wlc) {}
  void operator()(processor_t &, unsigned int) override;

private:
  reg_t read_total_counter;
  reg_t read_length_counter;
  reg_t logic_total_counter;
  reg_t write_total_counter;
  reg_t write_length_counter;
};

class profiler_t {
public:
  profiler_t();

  void run(processor_t &, const insn_t &, unsigned int) const;

private:
  std::unordered_map<uint32_t, std::unique_ptr<profiler_abs_t>> map;

  static constexpr reg_t read_byte_counter = CSR_MHPMCOUNTER3;
  static constexpr reg_t read_short_counter = CSR_MHPMCOUNTER4;
  static constexpr reg_t read_word_counter = CSR_MHPMCOUNTER5;
  static constexpr reg_t read_double_counter = CSR_MHPMCOUNTER6;
  static constexpr reg_t read_quad_counter = CSR_MHPMCOUNTER7;
  static constexpr reg_t read_counter = CSR_MHPMCOUNTER8;
  static constexpr reg_t write_byte_counter = CSR_MHPMCOUNTER9;
  static constexpr reg_t write_short_counter = CSR_MHPMCOUNTER10;
  static constexpr reg_t write_word_counter = CSR_MHPMCOUNTER11;
  static constexpr reg_t write_double_counter = CSR_MHPMCOUNTER12;
  static constexpr reg_t write_quad_counter = CSR_MHPMCOUNTER13;
  static constexpr reg_t write_counter = CSR_MHPMCOUNTER14;
  static constexpr reg_t integer_counter = CSR_MHPMCOUNTER15;
  static constexpr reg_t float_counter = CSR_MHPMCOUNTER16;
  static constexpr reg_t branch_counter = CSR_MHPMCOUNTER17;
};

#endif
