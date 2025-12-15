open Stdint
open Ast

let imm_out_of_range32 i = i > Uint64.of_int32 Int32.max_int

let fix_ins_cmp = function
  (* It is helpful to consider lhs as dst and rhs as src to better understand the semantics. *)
  | Cmp { lhs; rhs; sz } as ret ->
    let invalid_rhs =
      (* src, dst *)
      match rhs, lhs with
      | Imm i, _ when imm_out_of_range32 i -> true
      | (Stack _ | Data _), (Stack _ | Data _) -> true
      | _ -> false
    in
    let invalid_lhs =
      match lhs with
      | Imm _ -> true
      | _ -> false
    in
    if invalid_rhs && invalid_lhs
    then (
      let tmp_rhs = Reg R10 in
      let tmp_lhs = Reg R11 in
      [ Mov { src = rhs; dst = tmp_rhs; sz }
      ; Mov { src = lhs; dst = tmp_lhs; sz }
      ; Cmp { lhs = tmp_lhs; rhs = tmp_rhs; sz }
      ])
    else if invalid_rhs
    then (
      let tmp_rhs = Reg R10 in
      [ Mov { src = rhs; dst = tmp_rhs; sz }; Cmp { lhs; rhs = tmp_rhs; sz } ])
    else if invalid_lhs
    then (
      let tmp_lhs = Reg R11 in
      [ Mov { src = lhs; dst = tmp_lhs; sz }; Cmp { lhs = tmp_lhs; rhs; sz } ])
    else [ ret ]
  | _ -> assert false
;;

let fix_ins_binary = function
  | Binary { bop; src; dst; sz } as ret ->
    (match bop with
     | Add | Sub | And | Or | Xor ->
       let invalid_src =
         match src, dst with
         | Imm i, _ when imm_out_of_range32 i -> true
         | (Stack _ | Data _), (Stack _ | Data _) -> true
         | _ -> false
       in
       if invalid_src
       then (
         let tmp_src = Reg R10 in
         [ Mov { src; dst = tmp_src; sz }; Binary { bop; src = tmp_src; dst; sz } ])
       else [ ret ]
     | Imul ->
       let invalid_src =
         match src with
         | Imm i when imm_out_of_range32 i -> true
         | _ -> false
       in
       let invalid_dst =
         match dst with
         | Stack _ | Data _ -> true
         | _ -> false
       in
       if invalid_src && invalid_dst
       then (
         let tmp_src = Reg R10 in
         let tmp_dst = Reg R11 in
         [ Mov { src; dst = tmp_src; sz }
         ; Mov { src = dst; dst = tmp_dst; sz }
         ; Binary { bop; src = tmp_src; dst = tmp_dst; sz }
         ; Mov { src = tmp_dst; dst; sz }
         ])
       else if invalid_src
       then (
         let tmp_src = Reg R10 in
         [ Mov { src; dst = tmp_src; sz }; Binary { bop; src = tmp_src; dst; sz } ])
       else if invalid_dst
       then (
         let tmp_dst = Reg R11 in
         [ Mov { src = dst; dst = tmp_dst; sz }
         ; Binary { bop; src; dst = tmp_dst; sz }
         ; Mov { src = tmp_dst; dst; sz }
         ])
       else [ ret ]
     | Sal | Sar | Shr -> [ ret ])
  | _ -> assert false
;;

let fix_instruction = function
  | Mov { src; dst; sz } as ret ->
    (match src, dst with
     | (Stack _ | Data _), (Stack _ | Data _) ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; sz }; Mov { src = tmp_src; dst; sz } ]
     | Imm i, _ when imm_out_of_range32 i && sz = DWord ->
       (* Truncate out of range imm value. *)
       [ Mov { src = Imm (Uint64.of_int32 (Uint64.to_int32 i)); dst; sz } ]
     | Imm i, (Stack _ | Data _) when imm_out_of_range32 i ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; sz }; Mov { src = tmp_src; dst; sz } ]
     | _ -> [ ret ])
  | Movsx { src; dst } as ret ->
    (match src with
     | Imm _ ->
       (match dst with
        | Stack _ | Data _ ->
          let tmp_src = Reg R10 in
          let tmp_dst = Reg R11 in
          [ Mov { src; dst = tmp_src; sz = DWord }
          ; Movsx { src = tmp_src; dst = tmp_dst }
          ; Mov { src = tmp_dst; dst; sz = QWord }
          ]
        | _ ->
          let tmp_src = Reg R10 in
          [ Mov { src; dst = tmp_src; sz = DWord }; Movsx { src = tmp_src; dst } ])
     | _ ->
       (match dst with
        | Stack _ | Data _ ->
          let tmp_dst = Reg R11 in
          [ Movsx { src; dst = tmp_dst }; Mov { src = tmp_dst; dst; sz = QWord } ]
        | _ -> [ ret ]))
  | MovZeroExtend { src; dst } ->
    (match dst with
     | Reg _ -> [ Mov { src; dst; sz = DWord } ]
     | Stack _ | Data _ ->
       let tmp_dst = Reg R11 in
       [ Mov { src; dst = tmp_dst; sz = DWord }; Mov { src = tmp_dst; dst; sz = QWord } ]
     | Imm _ -> assert false)
  | Cmp _ as ret -> fix_ins_cmp ret
  | Binary _ as ret -> fix_ins_binary ret
  | Idiv (src, sz) as ret ->
    (match src with
     | Imm _ ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; sz }; Idiv (tmp_src, sz) ]
     | _ -> [ ret ])
  | Div (src, sz) as ret ->
    (match src with
     | Imm _ ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; sz }; Div (tmp_src, sz) ]
     | _ -> [ ret ])
  | Push (Imm i) when imm_out_of_range32 i ->
    let tmp_src = Reg R10 in
    [ Mov { src = Imm i; dst = tmp_src; sz = QWord }; Push tmp_src ]
  | _ as ret -> [ ret ]
;;
