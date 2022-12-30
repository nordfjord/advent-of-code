(* Makes a persistent string seq from stdin *)
let stdn_seq' = Lazy.from_val(Seq.of_dispenser(fun _ -> match read_line() with | x -> Some x | exception End_of_file -> None) 
  |> Array.of_seq
  |> Array.to_seq)
let stdin_seq () = Lazy.force stdn_seq'
  
