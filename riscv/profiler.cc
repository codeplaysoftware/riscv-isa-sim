// See LICENSE for license details.

#include "profiler.h"
#include "disasm.h"
#include "processor.h"

constexpr reg_t profiler_t::read_byte_counter;
constexpr reg_t profiler_t::read_short_counter;
constexpr reg_t profiler_t::read_word_counter;
constexpr reg_t profiler_t::read_double_counter;
constexpr reg_t profiler_t::read_quad_counter;
constexpr reg_t profiler_t::read_counter;
constexpr reg_t profiler_t::write_byte_counter;
constexpr reg_t profiler_t::write_short_counter;
constexpr reg_t profiler_t::write_word_counter;
constexpr reg_t profiler_t::write_double_counter;
constexpr reg_t profiler_t::write_quad_counter;
constexpr reg_t profiler_t::write_counter;
constexpr reg_t profiler_t::integer_counter;
constexpr reg_t profiler_t::float_counter;
constexpr reg_t profiler_t::branch_counter;

void profiler_memory_op_t::operator()(processor_t &p,
                                      const unsigned int offset) {
  auto reg_tc = total_counter + offset;
  auto reg_lc = length_counter + offset;
  auto tc = p.get_csr(reg_tc);
  auto lc = p.get_csr(reg_lc);

  p.put_csr(reg_tc, tc + 1);
  p.put_csr(reg_lc, lc + 1);
}

void profiler_logic_op_t::operator()(processor_t &p,
                                     const unsigned int offset) {
  auto reg_tc = total_counter + offset;
  auto tc = p.get_csr(reg_tc);

  p.put_csr(reg_tc, tc + 1);
}

void profiler_atomic_op_t::operator()(processor_t &p, unsigned int offset) {
  auto read_op = profiler_memory_op_t(read_total_counter, read_length_counter);
  auto logic_op = profiler_logic_op_t(logic_total_counter);
  auto write_op =
      profiler_memory_op_t(write_total_counter, write_length_counter);

  read_op(p, offset);
  logic_op(p, offset);
  write_op(p, offset);
}

void profiler_t::run(processor_t &p, const insn_t &insn,
                         const unsigned int xlen) const {
  auto match = p.get_disassembler()->lookup(insn)->get_match();
  auto pair = map.find(match);
  // use offset to select MHPMCOUNTER (0xB03) or MHPMCOUNTERH (0xB83) depending
  // on xlen
  auto offset = xlen == 32 ? 80 : 0;

  if (pair == map.end()) {
    return;
  }

  pair->second->operator()(p, offset);
}

profiler_t::profiler_t() {
  ////////////////////////////////////////////////////////////////////////////
  // RV32I EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // CONTROL TRANSFER INSTRUCTIONS
  map[MATCH_JAL] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_JALR] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BEQ] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BNE] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BLT] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BGE] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BLTU] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_BGEU] =
      std::make_unique<profiler_logic_op_t>(branch_counter);

  // LOAD INSTRUCTIONS
  map[MATCH_LB] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_LH] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_LW] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_LBU] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_LHU] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);

  // STORE INSTRUCTIONS
  map[MATCH_SB] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_byte_counter);
  map[MATCH_SH] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_SW] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_ADDI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLTI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLTIU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_XORI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_ORI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_ANDI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLLI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRLI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRAI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_ADD] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SUB] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLL] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLT] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLTU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_XOR] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRL] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRA] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_OR] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_AND] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  ////////////////////////////////////////////////////////////////////////////
  // RV64I EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_LWU] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_LD] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);

  // STORE INSTRUCTIONS
  map[MATCH_SD] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_ADDIW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLLIW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRLIW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRAIW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_ADDW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SUBW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SLLW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRLW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_SRAW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  ////////////////////////////////////////////////////////////////////////////
  // M EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOGIC INSTRUCTIONS
  map[MATCH_MUL] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_MULH] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_MULHSU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_MULHU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_MULW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_DIV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_DIVU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_DIVUW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_DIVW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_REM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_REMU] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_REMUW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_REMW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  ////////////////////////////////////////////////////////////////////////////
  // A EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_LR_W] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_LR_D] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);

  // STORE INSTRUCTIONS
  map[MATCH_SC_W] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_SC_D] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_AMOSWAP_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOSWAP_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);
  map[MATCH_AMOADD_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOADD_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);
  map[MATCH_AMOAND_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOAND_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);
  map[MATCH_AMOOR_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOOR_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);
  map[MATCH_AMOMAX_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOMAXU_W] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_word_counter,
      integer_counter, write_counter,
      write_word_counter);
  map[MATCH_AMOMAX_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);
  map[MATCH_AMOMAXU_D] = std::make_unique<profiler_atomic_op_t>(
      read_counter, read_double_counter,
      integer_counter, write_counter,
      write_double_counter);

  ////////////////////////////////////////////////////////////////////////////
  // F EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_FLH] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_FLW] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);

  // STORE INSTRUCTIONS
  map[MATCH_FSH] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_FSW] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_FMADD_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMSUB_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMSUB_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMADD_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FADD_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSUB_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMUL_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FDIV_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSQRT_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJ_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJN_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJX_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMIN_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMAX_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_W_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_WU_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMV_X_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FEQ_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLT_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLE_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCLASS_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_WU] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMV_W_X] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_L_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_LU_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_L] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_LU] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  ////////////////////////////////////////////////////////////////////////////
  // D EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_FLD] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);

  // STORE INSTRUCTIONS
  map[MATCH_FSD] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_FMADD_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMSUB_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMSUB_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMADD_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FADD_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSUB_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMUL_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FDIV_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSQRT_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJ_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJN_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJX_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMIN_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMAX_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_D_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FEQ_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLT_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLE_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCLASS_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_W_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_WU_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_D_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_D_WU] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  map[MATCH_FCVT_L_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_LU_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMV_X_D] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_D_L] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_D_LU] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMV_D_X] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  ////////////////////////////////////////////////////////////////////////////
  // Q EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_FLQ] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_quad_counter);

  // STORE INSTRUCTIONS
  map[MATCH_FSQ] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_quad_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_FMADD_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMSUB_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMSUB_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FNMADD_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FADD_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSUB_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMUL_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FDIV_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSQRT_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJ_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJN_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FSGNJX_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMIN_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FMAX_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_S_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_Q_S] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FEQ_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLT_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FLE_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCLASS_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_W_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_WU_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_Q_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_Q_WU] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_L_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_LU_Q] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_Q_L] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_FCVT_Q_LU] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  ////////////////////////////////////////////////////////////////////////////
  // C EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  map[MATCH_C_LWSP] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_C_LDSP] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_C_FLWSP] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_C_FLDSP] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_C_LW] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_C_LD] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_C_FLW] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_C_FLD] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);

  // STORE INSTRUCTIONS
  map[MATCH_C_SWSP] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_C_SDSP] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_C_FSWSP] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_C_FSDSP] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_C_SW] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_C_SD] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_C_FSW] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_C_FSD] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);

  // CONTROL TRANSFER INSTRUCTIONS
  map[MATCH_C_J] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_C_JAL] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_C_JR] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_C_JALR] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_C_BEQZ] =
      std::make_unique<profiler_logic_op_t>(branch_counter);
  map[MATCH_C_BNEZ] =
      std::make_unique<profiler_logic_op_t>(branch_counter);

  // LOGIC INSTRUCTIONS
  map[MATCH_C_LI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_LUI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADDI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADDIW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADDI16SP] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADDI4SPN] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_SLLI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_SRLI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_SRAI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ANDI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_MV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADD] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_AND] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_OR] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_XOR] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_SUB] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_ADDW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_C_SUBW] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  ////////////////////////////////////////////////////////////////////////////
  // V EXTENSION
  ////////////////////////////////////////////////////////////////////////////

  // LOAD INSTRUCTIONS
  // Vector Unit-Stride Instructions
  map[MATCH_VLE8_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_VLE16_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_VLE32_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_VLE64_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_VLE128_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE256_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE512_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE1024_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);

  // Vector Strided Instructions
  map[MATCH_VLSE8_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_VLSE16_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_VLSE32_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_VLSE64_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_VLSE128_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLSE256_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLSE512_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLSE1024_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);

  // Vector Indexed Instructions
  map[MATCH_VLUXEI8_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_VLUXEI16_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_VLUXEI32_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_VLUXEI64_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_VLUXEI128_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLUXEI256_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLUXEI512_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLUXEI1024_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLOXEI8_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_VLOXEI16_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_VLOXEI32_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_VLOXEI64_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_VLOXEI128_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLOXEI256_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLOXEI512_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLOXEI1024_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);

  //  Unit-stride Fault-Only-First Loads
  map[MATCH_VLE8FF_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_byte_counter);
  map[MATCH_VLE16FF_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_short_counter);
  map[MATCH_VLE32FF_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_word_counter);
  map[MATCH_VLE64FF_V] = std::make_unique<profiler_memory_op_t>(
      read_counter, read_double_counter);
  map[MATCH_VLE128FF_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE256FF_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE512FF_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);
  map[MATCH_VLE1024FF_V] =
      std::make_unique<profiler_logic_op_t>(read_counter);

  // STORE INSTRUCTIONS
  // Vector Unit-Stride Instructions
  map[MATCH_VSE8_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_byte_counter);
  map[MATCH_VSE16_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_VSE32_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_VSE64_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_VSE128_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSE256_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSE512_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSE1024_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);

  // Vector Strided Instructions
  map[MATCH_VSSE8_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_byte_counter);
  map[MATCH_VSSE16_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_VSSE32_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_VSSE64_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_VSSE128_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSSE256_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSSE512_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSSE1024_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);

  // Vector Indexed Instructions
  map[MATCH_VSUXEI8_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_byte_counter);
  map[MATCH_VSUXEI16_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_VSUXEI32_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_VSUXEI64_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_VSUXEI128_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSUXEI256_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSUXEI512_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSUXEI1024_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSOXEI8_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_byte_counter);
  map[MATCH_VSOXEI16_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_short_counter);
  map[MATCH_VSOXEI32_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_word_counter);
  map[MATCH_VSOXEI64_V] = std::make_unique<profiler_memory_op_t>(
      write_counter, write_double_counter);
  map[MATCH_VSOXEI128_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSOXEI256_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSOXEI512_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);
  map[MATCH_VSOXEI1024_V] =
      std::make_unique<profiler_logic_op_t>(write_counter);

  // LOGIC INSTRUCTIONS
  // Vector Single-Width Integer Add and Subtract
  map[MATCH_VADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADD_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSUB_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VRSUB_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VRSUB_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Widening Integer Add/Subtract
  map[MATCH_VWADDU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADDU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUBU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUBU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUB_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUB_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADDU_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADDU_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUBU_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUBU_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADD_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWADD_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUB_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWSUB_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Extension
  map[MATCH_VZEXT_VF2] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSEXT_VF2] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VZEXT_VF4] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSEXT_VF4] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VZEXT_VF8] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSEXT_VF8] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Add-with-Carry / Subtract-with-Borrow Instructions
  map[MATCH_VADC_VVM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADC_VXM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADC_VIM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMADC_VVM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMADC_VXM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMADC_VIM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSBC_VVM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSBC_VXM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSBC_VVM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSBC_VXM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Bitwise Logical Instructions
  map[MATCH_VADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VADD_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VOR_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VOR_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VOR_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VXOR_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VXOR_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VXOR_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Bit Shift Instructions
  map[MATCH_VSLL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSLL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSLL_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRL_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRA_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRA_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSRA_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Narrowing Integer Right Shift Instructions
  map[MATCH_VNSRL_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNSRL_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNSRL_WI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNSRA_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNSRA_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNSRA_WI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Comparison Instructions
  map[MATCH_VMSEQ_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSEQ_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSEQ_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSNE_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSNE_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSNE_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLTU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLTU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLT_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLT_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLEU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLEU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLEU_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLE_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLE_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSLE_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSGTU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSGTU_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSGT_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMSGT_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Min/Max Instructions
  map[MATCH_VMINU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMINU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMIN_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMIN_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMAXU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMAXU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMAX_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMAX_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Integer Multiply Instructions
  map[MATCH_VMUL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMUL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULH_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULH_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULHU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULHU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULHSU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMULHSU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Divide Instructions
  map[MATCH_VDIVU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VDIVU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VDIV_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VDIV_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VREMU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VREMU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VREM_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VREM_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Widening Integer Multiply Instructions
  map[MATCH_VWMUL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMUL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMULU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMULU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMULSU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMULSU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Integer Multiply-Add Instructions
  map[MATCH_VMACC_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMACC_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNMSAC_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNMSAC_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNMSUB_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNMSUB_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Widening Integer Multiply-Add Instructions
  map[MATCH_VWMACCU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACCU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACC_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACC_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACCSU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACCSU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VWMACCUS_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Merge Instructions
  map[MATCH_VMERGE_VVM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMERGE_VXM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMERGE_VIM] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Integer Move Instructions
  map[MATCH_VMV_V_V] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMV_V_X] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VMV_V_I] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Saturating Add and Subtract
  map[MATCH_VSADDU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSADDU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSADDU_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSADD_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSUBU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSUBU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSUB_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSUB_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Averaging Add and Subtract
  map[MATCH_VAADDU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VAADDU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VAADD_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VAADD_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VASUBU_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VASUBU_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VASUB_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VASUB_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Fractional Multiply with Rounding and Saturation
  map[MATCH_VSMUL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSMUL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Scaling Shift Instructions
  map[MATCH_VSSRL_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSRL_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSRL_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSRA_VV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSRA_VX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VSSRA_VI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Narrowing Fixed-Point Clip Instructions
  map[MATCH_VNCLIPU_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNCLIPU_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNCLIPU_WI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNCLIP_WV] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNCLIP_WX] =
      std::make_unique<profiler_logic_op_t>(integer_counter);
  map[MATCH_VNCLIP_WI] =
      std::make_unique<profiler_logic_op_t>(integer_counter);

  // Vector Single-Width Floating-Point Add/Subtract Instructions
  map[MATCH_VFADD_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFADD_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSUB_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSUB_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFRSUB_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Widening Floating-Point Add/Subtract Instructions
  map[MATCH_VFWADD_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWADD_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWSUB_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWSUB_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWADD_WV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWADD_WF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWSUB_WV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWSUB_WF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Single-Width Floating-Point Multiply/Divide Instructions
  map[MATCH_VFMUL_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMUL_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFDIV_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFDIV_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFRDIV_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Widening Floating-Point Multiply
  map[MATCH_VFWMUL_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWMUL_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Single-Width Floating-Point Fused Multiply-Add Instructions
  map[MATCH_VFMACC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMACC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMACC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMACC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMSAC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMSAC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMSAC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMSAC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMADD_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMADD_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMADD_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMADD_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMSUB_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMSUB_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMSUB_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNMSUB_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Widening Floating-Point Fused Multiply-Add Instructions
  map[MATCH_VFWMACC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWMACC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWNMACC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWNMACC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWMSAC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWMSAC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWNMSAC_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWNMSAC_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Square-Root Instruction
  map[MATCH_VFSQRT_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Reciprocal Square-Root Estimate Instruction
  map[MATCH_VFRSQRT7_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Reciprocal Estimate Instruction
  map[MATCH_VFREC7_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point MIN/MAX Instructions
  map[MATCH_VFMIN_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMIN_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMAX_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFMAX_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Sign-Injection Instructions
  map[MATCH_VFSGNJ_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSGNJ_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSGNJN_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSGNJN_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSGNJX_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFSGNJX_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Compare Instructions
  map[MATCH_VMFEQ_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFEQ_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFNE_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFNE_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFLT_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFLT_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFLE_VV] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFLE_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFGT_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VMFGE_VF] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Classify Instruction
  map[MATCH_VFCLASS_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Merge Instruction
  map[MATCH_VFMERGE_VFM] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Vector Floating-Point Move Instruction
  map[MATCH_VFMV_V_F] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Single-Width Floating-Point/Integer Type-Convert Instructions
  map[MATCH_VFCVT_XU_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFCVT_X_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFCVT_RTZ_XU_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFCVT_RTZ_X_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFCVT_F_XU_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFCVT_F_X_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Widening Floating-Point/Integer Type-Convert Instructions
  map[MATCH_VFWCVT_XU_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_X_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_RTZ_XU_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_RTZ_X_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_F_XU_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_F_X_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFWCVT_F_F_V] =
      std::make_unique<profiler_logic_op_t>(float_counter);

  // Narrowing Floating-Point/Integer Type-Convert Instructions
  map[MATCH_VFNCVT_XU_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_X_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_RTZ_XU_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_RTZ_X_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_F_XU_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_F_X_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_F_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
  map[MATCH_VFNCVT_ROD_F_F_W] =
      std::make_unique<profiler_logic_op_t>(float_counter);
}
