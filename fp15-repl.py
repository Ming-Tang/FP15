#!/usr/bin/env python
import os
import os.path as path
import readline
import subprocess as sp
import atexit
from fp15 import run_fp15

histfile = path.join(path.expanduser("~"), ".fp15-hist")

try:
    readline.read_history_file(histfile)
except IOError:
    pass

atexit.register(readline.write_history_file, histfile)
del histfile

defs = []

def parse_cmd(l):
    cmds = [":d", ":s", ":e", ":x"]
    for cmd in cmds:
        if l.startswith(cmd + " ") or l == cmd:
            return l[:len(cmd)], l[len(cmd) + 1:]

    return None, l

while True:
    try:
        cmd, code = parse_cmd(raw_input("> "))

        if cmd == ":x":
            break
        elif cmd == ":d":
            defs.append(code)
        elif cmd == ":s":
            print "\n".join(defs)
        elif cmd == ":e":
            try:
                defs.pop(min(int(code) + 1, 0))
            except ValueError:
                print("Please enter a line number to delete.")
                pass
            except IndexError:
                pass
        else:
            run_fp15("\n".join(defs + ["main = " + code]))
    except EOFError:
        print("")
        break
    except KeyboardInterrupt:
        print("<Interrupt>")

