#!/usr/bin/env python3

import subprocess
import os

challenger = "./f10 ../grading/{}"
adversary = "./f11 ../grading/{}"

# c=subprocess.run("./lexer {} | ./parser | ./semant",stdout=subprocess.PIPE,shell=True)
# print(c.stdout)

# exit()

for i, j, k in os.walk("../grading"):
    for f in k:
        if f.endswith(".cl") or f.endswith(".cool"):
            print(f)
            c = subprocess.run(challenger.format(
                f), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            a = subprocess.run(adversary.format(f), shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            if c.stderr != b"":
                if c.stderr != a.stderr:
                    print(c.stderr)
                    input()
            print(f, c.stdout == a.stdout)
            if c.stdout != a.stdout:
                # print(open("../grading/"+f, "rb").read())
                open("challenger.txt", "wb").write(c.stdout)
                open("adversary.txt", "wb").write(a.stdout)
                input()
