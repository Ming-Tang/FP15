#!/usr/bin/env python
import os
import os.path as path
from fp15 import run_fp15

# TODO integrate with REPL, refactor REPL into a class

def parse_test_case(contents):
    code, out = [], []
    state = 0
    for line in contents.splitlines():
        if state == 0 and line.startswith(">>> "):
            if len(code):
                yield "\n".join(code), "\n".join(out)

            code, out = [], []
            state = 1
            code.append(line[4:])
        elif state == 1 and line.startswith("... "):
            code.append(line[4:])
        elif state == 1:
            state = 0
            out.append(line)
        else:
            raise Exception(
                "Invalid file format. {!r}".format(
                    dict(line=line, code=code, out=out,
                         state=state, contents=contents)))


test_case_path = path.join(path.dirname(path.realpath(__file__)),
                           './test-cases')

runs, fails = 0, 0
files = [f for f in os.listdir(test_case_path) if f.endswith(".txt")]

for filename in files:
    print "--- {} ---".format(filename)
    f = path.join(test_case_path, filename)

    for code, expected_out in parse_test_case(''.join(open(f, 'r'))):
        out, err = run_fp15("main = {}".format(code), redirect=True)
        print ">>> {}".format(code)
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
