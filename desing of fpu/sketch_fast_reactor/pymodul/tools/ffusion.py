import os

def ffusion(arg,target,par):
    full_content = ""
    if os.path.exists(target):
        handle = open(target,par)
    else:
        handle = open(target,"w")
    for line in arg:
        for content in open(line,"r"):
            full_content = full_content + content
    handle.write(full_content)
    handle.close()
    pass
