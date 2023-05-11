#!/usr/bin/env python3
import subprocess
import os.path
import shutil
import re
import sys

def gather_deps():
    output = subprocess.check_output(["alr", "show", "--solve"]).decode(
            "utf-8", "replace").split("\n")
    
    ret = []

    in_solution = False
    for line in output:
        if line.startswith ("Dependencies (solution)"):
            in_solution = True
        elif in_solution:
            if line.startswith ("Dependencies (graph)"):
                return ret
            match = re.search('\s*(?P<name>[a-z_]+)=(?P<version>[0-9.\-a-z]+)\s*(?P<pinned>\(pinned\))?\s*(?P<actual>\([a-z_]+\))?\s*(?:\(installed\))?', line)
            if match:
                name = match.group('name')
                version = match.group('version')
                if match.group('actual'):
                    name = match.group('actual').strip(')').strip('(')

                pinned = match.group('pinned') != None

                ret += [{'name':name, 'version':version, 'pinned': pinned}]

    return ret


def alr_show(crate, external=False):
    args = ['alr', 'show', crate]

    if external:
        args += ['--external']

    return subprocess.check_output(args).decode("utf-8", "replace").split("\n")


def gather_info(packages):
    ret = []
    for dep in packages:
        if not dep['pinned']:
            name = dep['name']
            version = dep['version']
            output = alr_show("%s=%s" % (name, version))
                
            if output[0].startswith("Not found:"):
                output = alr_show("%s=%s" % (name, version), external=True)
                
            output.insert(0, "-" * 80 + "\n")
            output.insert(0, "-- %s=%s\n" % (name, version))
            output.insert(0, "-" * 80 + "\n")
            ret += output

    return ret


def main():
    all_deps = gather_deps()

    output = gather_info(all_deps)
    output.insert(0, "(https://alire.ada.dev)\n")
    output.insert(0, "Dependencies from the Alire package manager:\n")
    
    if sys.argv[1]:
        with open(sys.argv[1], 'a') as f:
            for line in output:
                f.write(line)
    else:
        for line in output:
            print(line)
    
if __name__ == "__main__":
    main()
