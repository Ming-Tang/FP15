#!/usr/bin/env python
import os
import os.path as path
from fp15 import run_fp15


def parse_test_case(lines):
    code, out = [], []
    state = 0
    indices = [i for i, line in enumerate(lines) if line.startswith(">>> ")]
    if indices and indices[0] != 0:
        raise Exception("First line must be a >>> ")

    join = '\n'.join
    for i, index in enumerate(indices):
        mid_index = index
        while mid_index + 1 < len(lines) and lines[mid_index + 1].startswith('... '):
            mid_index += 1

        next_index = indices[i + 1] if i + 1 < len(indices) else len(lines)
        code = join(line[4:] for line in lines[index : mid_index + 1])
        out = join(lines[mid_index + 1 : next_index])
        yield (code, out)

test_case_path = path.join(path.dirname(path.realpath(__file__)),
                           './test-cases')

runs, fails = 0, 0
files = [f for f in os.listdir(test_case_path) if f.endswith(".txt")]

for filename in files:
    print "--- {} ---".format(filename)
    f = path.join(test_case_path, filename)
    lines = []
    with open(f, 'r') as f: lines = [line.strip() for line in f.readlines()]
    for code, expected_out in parse_test_case(lines):
        out, err = run_fp15("main = {}".format(code), redirect=True)
        print ">>> {}".format(code.replace('\n', '\n... '))
        print "{}".format(expected_out)
        expected_out += "\n"

        runs += 1
        assert not err, (code, err)
        if out != expected_out:
            print "*** FAIL: Actual output is:"
            print out
            fails += 1

    print

print "(fails, runs) = ({}, {})".format(fails, runs)
raise SystemExit(fails > 0)
