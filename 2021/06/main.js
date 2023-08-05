function simulateNaive(fishes, days) {
  if (days === 0) return fishes.length

  return simulateNaive(
    fishes.flatMap((fish) => (fish === 0 ? [6, 8] : fish - 1)),
    days - 1
  )
}

function simulate(fishes, days) {
  const state = new Array(9).fill(0)

  for (const fish of fishes) state[fish]++

  for (let day = 0; day < days; day++) {
    let zeroes = state[0]
    state[0] = state[1]
    state[1] = state[2]
    state[2] = state[3]
    state[3] = state[4]
    state[4] = state[5]
    state[5] = state[6]
    state[6] = state[7] + zeroes
    state[7] = state[8]
    state[8] = zeroes
  }

  return state.reduce((a, b) => a + b, 0)
}

console.log(simulate([3, 4, 3, 1, 2], 80))
console.log(simulate([3, 4, 3, 1, 2], 256))

console.log(simulateNaive([3, 4, 3, 1, 2], 80))
console.log(simulateNaive([3, 4, 3, 1, 2], 256))
