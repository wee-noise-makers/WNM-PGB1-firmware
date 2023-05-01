#!/usr/bin/env python3

# based on mingw-bundledlls.py by: Martin Preisler
#
# The MIT License (MIT)
#
# Copyright (c) 2015 Martin Preisler
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.


import subprocess
import os.path
import argparse
import shutil
import re


def gather_deps(path):
    ret = []
    output = subprocess.check_output(["ldd", path]).decode(
        "utf-8", "replace").split("\n")
    for line in output:
        result = re.search(r"(.*) => (.*) (\(0x[0-9a-f]+\))", line)
        if not result:
            continue

        dep  = result.group(1)
        dep_path = result.group(2)
    
        if dep_path.lower().startswith("/c/windows/"):
            continue

        if dep_path.startswith("/mingw64/"):
            dep_path = "/c/msys2" + dep_path

        ret.append(dep_path)

    return ret


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "exe_file",
        help="EXE or DLL file that you need to bundle dependencies for"
    )
    parser.add_argument(
        "--copy",
        action="store_true",
        help="In addition to printing out the dependencies, also copy them next to the exe_file"
    )
    parser.add_argument(
        "--upx",
        action="store_true",
        help="Only valid if --copy is provided. Run UPX on all the DLLs and EXE."
    )
    args = parser.parse_args()

    if args.upx and not args.copy:
        raise RuntimeError("Can't run UPX if --copy hasn't been provided.")

    all_deps = set(gather_deps(args.exe_file))

    print("\n".join(all_deps))

    if args.copy:
        print("Copying enabled, will now copy all dependencies next to the exe_file.\n")

        parent_dir = os.path.dirname(os.path.abspath(args.exe_file))

        for dep in all_deps:
            target = os.path.join(parent_dir, os.path.basename(dep))

            try:
                print("Copying '%s' to '%s'" % (dep, target))
                output = subprocess.check_output(["cp", dep, target]).decode(
                    "utf-8", "replace").split("\n")

                # shutil.copy(dep, parent_dir)

            except shutil.SameFileError:
                print("Dependency '%s' was already in target directory, "
                      "skipping..." % (dep))

            if args.upx:
                subprocess.call(["upx", target])


if __name__ == "__main__":
    main()
