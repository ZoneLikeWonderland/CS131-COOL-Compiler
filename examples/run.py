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
        if f.endswith(".cl") or f.endswith(".cool"):
            c = subprocess.run(challenger.format(
                f), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            if c.stderr != b"":
                print(f)
                s = {i[:i.find(" ")]: i[i.find(" ")+1:]
                     for i in c.stderr.decode().split("\n")}
                # print(s)
                a = subprocess.run(adversary.format(
                    f), shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                t = {i[:i.find(" ")]: i[i.find(" ")+1:]
                     for i in a.stderr.decode().split("\n")}
                center = {i for i in s} | {i for i in t}
                for i in center:
                    try:
                        print("\033[32m"+i+s[i]+"\033[0m")
                    except:
                        pass
                    try:
                        print("\033[31m"+i+t[i]+"\033[0m")
                    except:
                        pass
                if s != t:
                    input()
                continue
            a = subprocess.run(adversary.format(f), shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            print(f, c.stdout == a.stdout)
            if c.stdout != a.stdout:
                print(open(f, "rb").read())
                open("challenger.txt", "wb").write(c.stdout)
                open("adversary.txt", "wb").write(a.stdout)
                input()
