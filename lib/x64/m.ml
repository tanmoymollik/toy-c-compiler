open Ast

let gen_program tacky_prog =
  let prog = Gen.gen_program tacky_prog in
  match prog with
  | Program tpns ->
    let tpns =
      List.map (fun tpn -> RegAlloc.resolve_top_level tpn |> InsFixer.fix_top_level) tpns
    in
    Program tpns
;;
