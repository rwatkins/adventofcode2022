import re

with open("day4_input.txt") as f:
    input = f.read()

lines = input.splitlines()
pairs = [re.split(r"[-,]", line) for line in lines]
pairs = [[int(i) for i in pair] for pair in pairs]

count = 0
for l_start, l_end, r_start, r_end in pairs:
    if l_start >= r_start and l_end <= r_end or r_start >= l_start and r_end <= l_end:
        count += 1

print("Part 1:", count)

count = 0
for l_start, l_end, r_start, r_end in pairs:
    if set(range(l_start, l_end + 1)) & set(range(r_start, r_end + 1)):
        count += 1

print("Part 2:", count)
