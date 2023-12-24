import sys
import re
from copy import deepcopy

inp = sys.stdin.read().splitlines()

lines = []
for l in inp:
  pos, vel = l.split('@')
  x,y,z = pos.split(', ')
  vx,vy,vz = vel.split(', ')
  x,y,z = int(x),int(y),int(z)
  vx,vy,vz = int(vx),int(vy),int(vz)
  lines.append((x,y,z,vx,vy,vz))


from z3 import *
x,y,z,vx,vy,vz = Int('x'),Int('y'),Int('z'),Int('vx'),Int('vy'),Int('vz')
solver = Solver()
for i in range(len(lines)):
  xi,yi,zi,vxi,vyi,vzi = lines[i]
  t = Int(f't{i}')
  # x at time t = x' at time t
  solver.add(x + t*vx == xi + t*vxi)
  solver.add(y + t*vy == yi + t*vyi)
  solver.add(z + t*vz == zi + t*vzi)
res = solver.check()
model = solver.model()

print(model[x], model[y], model[z])
print(model[vx], model[vy], model[vz])
print(model.eval(x+y+z))
