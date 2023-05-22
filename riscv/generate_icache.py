#!/usr/bin/env python3

import sys
import re

def main(argv):
    if len(argv) != 3:
        sys.stderr.write("usage: {} <MMU header> <output header>\n".format(argv[0]))
        return 1
    input_header_path = argv[1]
    output_header_path = argv[2]

    # Read the input header.
    entries_regex = re.compile(r"ICACHE_ENTRIES = (\d+);\s*$")
    num_icache_entries = None
    with open(input_header_path, "r") as f:
        for line in f:
            m = entries_regex.search(line)
            if m:
                num_icache_entries = int(m.group(1))
                break
    if num_icache_entries is None:
        sys.stderr.write("error: could not find 'ICACHE_ENTRIES' definition in '{}'\n".format(input_header_path))
        return 1

    # Generate the output header.
    with open(output_header_path, "w") as f:
        for i in range(0, num_icache_entries):
            f.write("case {0}: ICACHE_ACCESS({0});\n".format(i))

    return 0

if __name__ == "__main__":
    sys.exit(main(sys.argv))
