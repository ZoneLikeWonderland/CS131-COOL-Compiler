import subprocess
import os

ancient="./lexer"

challenger="./parser"
adversary="./parser2"


for i,j,k in os.walk("."):
    for f in k:
        if f[-3:]!=".cl" and f[-5:]!=".cool":
            continue
        
        begin=subprocess.run([ancient,f],stdout=subprocess.PIPE,)

        # c=subprocess.run([challenger,f],stdout=subprocess.PIPE)
        # a=subprocess.run([adversary,f],stdout=subprocess.PIPE)
        # print(f,c.stdout==a.stdout)
        # if c.stdout!=a.stdout :
        #     print(open(f,"rb").read())
        #     open("challenger.txt","wb").write(c.stdout)
        #     open("adversary.txt","wb").write(a.stdout)
        #     input()
        # print(begin.stdout)
        # input()
        print("challenger running")
        p=subprocess.Popen([challenger,"-v"],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
        c = p.communicate(input=begin.stdout)[0]


        print("adversary running")
        p=subprocess.Popen([adversary,"-v"],stdin=subprocess.PIPE,stdout=subprocess.PIPE)
        a = p.communicate(input=begin.stdout)[0]
        
        print(f,c==a)
        if a!=c:
            print(begin.stdout.decode())
            open("challenger.txt","wb").write(c)
            open("adversary.txt","wb").write(a)
            input()
        print()