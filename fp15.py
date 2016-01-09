import os.path as path
import subprocess as sp

executable_path = path.join(path.dirname(path.realpath(__file__)),
                            './dist/build/FP15/FP15')

def run_fp15(code, redirect=False):
    fp15 = None
    try:
        args = dict(stdout=sp.PIPE, stderr=sp.PIPE) if redirect else {}
        fp15 = sp.Popen([executable_path], stdin=sp.PIPE,
                        **args)
        return fp15.communicate(code)
    except KeyboardInterrupt:
        if fp15 is not None:
            fp15.terminate()
        raise

# print "{!r}".format(run_fp15("main = 123", redirect=True))

