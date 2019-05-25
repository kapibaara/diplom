strings = []

with open("sokr.txt") as fd:
  for line in fd:
    strings.append(line)
    
strings.sort()

with open("newsokr.txt", "w") as fd:
  for line in strings:
    fd.write(line)