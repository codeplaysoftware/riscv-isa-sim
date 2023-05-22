#!/usr/bin/env python3

import os
import sys
import re

def read_encoding_table(path):
    instructions = {}
    with open(path, "r") as f:
        for line in f:
            m = re.match("DECLARE_INSN\((\w+), (\w+), (\w+)\)", line)
            if not m:
                continue
            instructions[m.group(1)] = m.groups()
    return instructions


def load_template(template_path):
    with open(template_path, "r") as f:
        template = f.read()

    # Separate the body of the template from the header.
    lines = template.split("\n")
    body_start_line = None
    for i, line in enumerate(lines):
        if line.find("(") >= 0:
            body_start_line = i
            break
    if body_start_line is not None:
        return "\n".join(lines[0:body_start_line]), "\n".join(lines[body_start_line:])
    else:
        return "", template


def generate_from_template(template, instruction):
    output = template.replace("NAME", instruction[0])
    output = output.replace("OPCODE", instruction[1])
    return output


def main(argv):
    if len(argv) != 4 and len(argv) != 5:
        sys.stderr.write("usage: {} <source dir> <output source> <output header> [<instruction name>]\n".format(argv[0]))
        return 1
    source_dir = argv[1]
    output_path = argv[2]
    header_path = argv[3]
    instruction_name = None
    if len(argv) >= 5:
        instruction_name = argv[4]

    # Read the template.
    template_file = os.path.join(source_dir, "insn_template.cc")
    if not os.path.exists(template_file):
        sys.stderr.write("error: file not found: '{}'\n".format(template_file))
        return 1
    template_header, template_body = load_template(template_file)

    # Read the instruction table.
    encoding_file = os.path.join(source_dir, "encoding.h")
    if not os.path.exists(encoding_file):
        sys.stderr.write("error: file not found: '{}'\n".format(encoding_file))
        return 1
    instructions = read_encoding_table(encoding_file)

    # Filter instructions.
    names = list(instructions.keys())
    for name in names:
        if not os.path.exists(os.path.join(source_dir, "insns", "{}.h".format(name))):
            del instructions[name]

    output = None
    if instruction_name:
        # Look the instruction up.
        try:
            instruction = instructions[instruction_name]
        except KeyError:
            sys.stderr.write("error: instruction not found in encoding table: '{}'\n".format(instruction_name))
            return 1

        # Generate code for this instruction.
        output = generate_from_template(template_body, instruction)
    else:
        # Generate code for all instructions.
        chunks = []
        for instruction in instructions.values():
            chunks.append(generate_from_template(template_body, instruction))

            # Sometimes the template sets this CHECK_REG macro and messes up
            # future generated instructions. This is an issue here as
            # we generate one big file. This is different to spike's
            # makefiles, where lots of files are generated. We should consider
            # moving to that in the future. For now just unset the macros
            # after each instruction.
            chunks.append("#undef CHECK_REG")
            chunks.append("#define CHECK_REG(reg) ((void)0)\n")
        output = "\n".join(chunks)

    # Write the source file.
    with open(output_path, "w") as f:
        if template_header:
            f.write(template_header + "\n")
        f.write(output)

    # Write the header file.
    with open(header_path, "w") as f:
        for name in sorted(instructions.keys()):
            f.write("DEFINE_INSN({})\n".format(name))

    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
