import fs from 'fs/promises'
import { exec } from 'child_process'

const day = process.argv[2]
const year = process.argv[3] || '2023'

const cookie = await fs.readFile('./cookie.txt', 'utf-8')

const input = await fetch(`https://adventofcode.com/${year}/day/${day}/input`, {
  headers: { cookie },
}).then((res) => res.text())

const dunefile = `(executable
 (name solution)
 (preprocess (pps ppx_deriving.show ppx_jane))
 (libraries str base stdio parsexp))

(env
 (dev
  (flags
   (:standard -warn-error -A))))
`

const solutionfile = `open Base
open Stdio

let lines = In_channel.input_lines stdin

let () =
  lines |> List.length |> printf "%d\\n"
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
