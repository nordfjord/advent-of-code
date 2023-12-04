import sys
from collections import defaultdict

lines = sys.stdin.read().splitlines()
p1 = 0
N = defaultdict(int)

for i,line in enumerate(lines):
  N[i] += 1
  first,rest = line.split('|')
  id,card = first.split(':')
  card_nums = [int(x) for x in rest.split()]
  winning_nums = [int(x) for x in card.split()]
  val = len(set(card_nums) & set(winning_nums))
  if val > 0:
    p1 += 2**(val-1)
  for j in range(val):
    N[i + j + 1] += N[i]

print(p1)
print(sum(N.values()))
