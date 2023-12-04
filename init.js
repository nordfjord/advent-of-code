import fs from 'fs/promises'
import { exec } from 'child_process'

const day = process.argv[2]

const cookie = await fs.readFile('./cookie.txt', 'utf-8')

const input = await fetch(`https://adventofcode.com/2023/day/${day}/input`, {
  headers: { cookie },
}).then((res) => res.text())

const dunefile = `(executable
 (name solution)
 (preprocess (pps ppx_deriving.show))
 (libraries str angstrom))

(env
 (dev
  (flags
   (:standard -warn-error -A))))
`

const solutionfile = `let lines =
  Seq.of_dispenser (fun _ ->
    match read_line () with
    | x -> Some x
    | exception End_of_file -> None)
  |> Array.of_seq
`

const day_ = day.padStart(2, '0')

if (!(await fs.stat(`./2023/${day_}`).catch(() => false))) {
  await fs.mkdir(`./2023/${day_}`)
}

await Promise.all([
  fs.writeFile(`./2023/${day_}/dune`, dunefile),
  fs.writeFile(`./2023/${day_}/solution.ml`, solutionfile),
  fs.writeFile(`./2023/${day_}/input.txt`, input.trim()),
])

await exec('dune build', {
  stdio: 'inherit',
})
