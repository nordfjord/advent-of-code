import fs from 'fs/promises'
import { exec } from 'child_process'

const day = process.argv[2]
const year = process.argv[3] || '2025'

const cookie = await fs.readFile('./cookie.txt', 'utf-8')

const input = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
  headers: { cookie },
}).then((res) => res.text())

const dunefile = `(executable
 (name solution)
 (preprocess (pps ppx_deriving.show ppx_jane))
 (libraries str base stdio parsexp prelude))

(env
 (dev
  (flags
   (:standard -warn-error -A))))
`

const solutionfile = `open Base
open Stdio

let start = Time_now.nanosecond_counter_for_timing ()

let lines = In_channel.input_lines stdin

let part1 () = 0
let part2 () = 0

let () =
  Prelude.Runner.run part1 part2;
  let stop = Time_now.nanosecond_counter_for_timing () in
  printf "Execution time: %.3f ms\n" (Int63.to_float Int63.(stop - start) /. 1_000_000.)
`

const day_ = day.padStart(2, '0')

if (!(await fs.stat(`./${year}/${day_}`).catch(() => false))) {
  await fs.mkdir(`./${year}/${day_}`)
}

await Promise.all([
  fs.writeFile(`./${year}/${day_}/dune`, dunefile),
  fs.writeFile(`./${year}/${day_}/solution.ml`, solutionfile),
  fs.writeFile(`./${year}/${day_}/input.txt`, input.trim()),
])

await exec('dune build', {
  stdio: 'inherit',
})
