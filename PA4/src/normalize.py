import re

order=["Object","IO","Int","Bool","String"]

f=open("0.s").read()

stay=f[:f.find("Object_dispTab:")]

a=f[f.find("Object_dispTab:"):f.find("	.word	-1\nObject_protObj:")]
b=f[f.find("	.word	-1\nObject_protObj:"):f.find("	.globl	heap_start")]
mid=f[f.find("	.globl	heap_start"):f.find("Object_init:")]
c=f[f.find("Object_init:"):f.find("Main.main:")]

last=f[f.find("Main.main:"):]

def reorder(a,suffix,ref=".+:"):
    aa=[a.find(i) for i in re.findall(ref,a)]+[999999]
    aaa=[]
    for i in range(len(aa)-1):
        aaa.append(a[aa[i]:aa[i+1]])
    aaaa=[]
    for i in order:
        for j in aaa:
            if j.find(i+suffix+":")!=-1:
                aaaa.append(j)
                aaa.remove(j)
                break
    aaaa.extend(aaa)
    aaaaa="".join(aaaa)
    return aaaaa




g=open("0.s","w")
g.write(stay+
    reorder(a,"_dispTab")+
    reorder(b,"_protObj","	.word	-1\n.+:")+
    mid+
    reorder(c,"_init")+
    last)