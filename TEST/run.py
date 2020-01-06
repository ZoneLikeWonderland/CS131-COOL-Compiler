#!/usr/bin/env python3

import subprocess
import os

challenger = "./lexer {} | ./parser | ./semant"
adversary = "./lexer {} | ./parser | ./mysemant"

# c=subprocess.run("./lexer {} | ./parser | ./semant",stdout=subprocess.PIPE,shell=True)
# print(c.stdout)

# exit()

for i, j, k in os.walk("."):
    for f in k:
        # if f.endswith(".cl") or f.endswith(".cool"):
        if f.endswith(".test"):
            c = subprocess.run(challenger.format(
                f), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            if c.stderr != b"":
                print(f)
                s = {i
                     for i in c.stderr.decode().split("\n")}
                # print(s)
                a = subprocess.run(adversary.format(
                    f), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                t = {i
                     for i in a.stderr.decode().split("\n")}
                center = s | t
                for i in center:
                    if i in s:
                        print("\033[32m"+i+"\033[0m")
                    if i in t:
                        print("\033[31m"+i+"\033[0m")

                # print(c.stderr.decode())
                # print(a.stderr.decode())
                if s != t:
                    input()
                continue
            a = subprocess.run(adversary.format(f), shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            print(f, c.stdout == a.stdout)
            if c.stdout != a.stdout:
                print(open(f, "rb").read())
                open("__challenger.txt", "wb").write(c.stdout)
                open("__adversary.txt", "wb").write(a.stdout)
                input()
