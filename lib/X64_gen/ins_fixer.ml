let imm_out_of_range32 i = i > Int64.of_int32 Int32.max_int

let fix_ins_cmp = function
  (* It is helpful to consider lhs as dst and rhs as src to better understand the semantics. *)
  | X64_ast.Cmp { lhs; rhs; sz } as ret ->
    let invalid_rhs =
      (* src, dst *)
      match rhs, lhs with
      | X64_ast.Imm i, _ when imm_out_of_range32 i -> true
      | (X64_ast.Stack _ | X64_ast.Data _), (X64_ast.Stack _ | X64_ast.Data _) -> true
      | _ -> false
    in
    let invalid_lhs =
      match lhs with
      | X64_ast.Imm _ -> true
      | _ -> false
    in
    if invalid_rhs && invalid_lhs
    then (
      let tmp_rhs = X64_ast.Reg X64_ast.R10 in
      let tmp_lhs = X64_ast.Reg X64_ast.R11 in
      [ X64_ast.Mov { src = rhs; dst = tmp_rhs; sz }
      ; X64_ast.Mov { src = lhs; dst = tmp_lhs; sz }
      ; X64_ast.Cmp { lhs = tmp_lhs; rhs = tmp_rhs; sz }
      ])
    else if invalid_rhs
    then (
      let tmp_rhs = X64_ast.Reg X64_ast.R10 in
      [ X64_ast.Mov { src = rhs; dst = tmp_rhs; sz }
      ; X64_ast.Cmp { lhs; rhs = tmp_rhs; sz }
      ])
    else if invalid_lhs
    then (
      let tmp_lhs = X64_ast.Reg X64_ast.R11 in
      [ X64_ast.Mov { src = lhs; dst = tmp_lhs; sz }
      ; X64_ast.Cmp { lhs = tmp_lhs; rhs; sz }
      ])
    else [ ret ]
  | _ -> assert false
;;

let fix_ins_binary = function
  | X64_ast.Binary { bop; src; dst; sz } as ret ->
    (match bop with
     | X64_ast.Add | X64_ast.Sub | X64_ast.And | X64_ast.Or | X64_ast.Xor ->
       let invalid_src =
         match src, dst with
         | X64_ast.Imm i, _ when imm_out_of_range32 i -> true
         | (X64_ast.Stack _ | X64_ast.Data _), (X64_ast.Stack _ | X64_ast.Data _) -> true
         | _ -> false
       in
       if invalid_src
       then (
         let tmp_src = X64_ast.Reg X64_ast.R10 in
         [ X64_ast.Mov { src; dst = tmp_src; sz }
         ; X64_ast.Binary { bop; src = tmp_src; dst; sz }
         ])
       else [ ret ]
     | X64_ast.Imul ->
       let invalid_src =
         match src with
         | Imm i when imm_out_of_range32 i -> true
         | _ -> false
       in
       let invalid_dst =
         match dst with
         | X64_ast.Stack _ | X64_ast.Data _ -> true
         | _ -> false
       in
       if invalid_src && invalid_dst
       then (
         let tmp_src = X64_ast.Reg X64_ast.R10 in
         let tmp_dst = X64_ast.Reg X64_ast.R11 in
         [ X64_ast.Mov { src; dst = tmp_src; sz }
         ; X64_ast.Mov { src = dst; dst = tmp_dst; sz }
         ; X64_ast.Binary { bop; src = tmp_src; dst = tmp_dst; sz }
         ; X64_ast.Mov { src = tmp_dst; dst; sz }
         ])
       else if invalid_src
       then (
         let tmp_src = X64_ast.Reg X64_ast.R10 in
         [ X64_ast.Mov { src; dst = tmp_src; sz }
         ; X64_ast.Binary { bop; src = tmp_src; dst; sz }
         ])
       else if invalid_dst
       then (
         let tmp_dst = X64_ast.Reg X64_ast.R11 in
         [ X64_ast.Mov { src = dst; dst = tmp_dst; sz }
         ; X64_ast.Binary { bop; src; dst = tmp_dst; sz }
         ; X64_ast.Mov { src = tmp_dst; dst; sz }
         ])
       else [ ret ]
     | X64_ast.Sal | X64_ast.Sar -> [ ret ])
  | _ -> assert false
;;

let fix_instruction = function
  | X64_ast.Mov { src; dst; sz } as ret ->
    (match src, dst with
     | (X64_ast.Stack _ | X64_ast.Data _), (X64_ast.Stack _ | X64_ast.Data _) ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src; sz }; X64_ast.Mov { src = tmp_src; dst; sz } ]
     | X64_ast.Imm i, _ when i > Int64.of_int32 Int32.max_int && sz = X64_ast.DWord ->
       [ X64_ast.Mov { src = X64_ast.Imm (Int64.of_int32 (Int64.to_int32 i)); dst; sz } ]
     | X64_ast.Imm i, (X64_ast.Stack _ | X64_ast.Data _)
       when i > Int64.of_int32 Int32.max_int ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src; sz }; X64_ast.Mov { src = tmp_src; dst; sz } ]
     | _ -> [ ret ])
  | X64_ast.Movsx { src; dst } as ret ->
    (match src with
     | X64_ast.Imm _ ->
       (match dst with
        | X64_ast.Stack _ | X64_ast.Data _ ->
          let tmp_src = X64_ast.Reg X64_ast.R10 in
          let tmp_dst = X64_ast.Reg X64_ast.R11 in
          [ X64_ast.Mov { src; dst = tmp_src; sz = X64_ast.DWord }
          ; X64_ast.Movsx { src = tmp_src; dst = tmp_dst }
          ; X64_ast.Mov { src = tmp_dst; dst; sz = X64_ast.QWord }
          ]
        | _ ->
          let tmp_src = X64_ast.Reg X64_ast.R10 in
          [ X64_ast.Mov { src; dst = tmp_src; sz = X64_ast.DWord }
          ; X64_ast.Movsx { src = tmp_src; dst }
          ])
     | _ ->
       (match dst with
        | X64_ast.Stack _ | X64_ast.Data _ ->
          let tmp_dst = X64_ast.Reg X64_ast.R11 in
          [ X64_ast.Movsx { src; dst = tmp_dst }
          ; X64_ast.Mov { src = tmp_dst; dst; sz = X64_ast.QWord }
          ]
        | _ -> [ ret ]))
  | X64_ast.Cmp _ as ret -> fix_ins_cmp ret
  | X64_ast.Binary _ as ret -> fix_ins_binary ret
  | X64_ast.Idiv (src, sz) as ret ->
    (match src with
     | X64_ast.Imm _ ->
       let tmp_src = X64_ast.Reg X64_ast.R10 in
       [ X64_ast.Mov { src; dst = tmp_src; sz }; X64_ast.Idiv (tmp_src, sz) ]
     | _ -> [ ret ])
  | X64_ast.Push (X64_ast.Imm i) when i > Int64.of_int32 Int32.max_int ->
    let tmp_src = X64_ast.Reg X64_ast.R10 in
    [ X64_ast.Mov { src = X64_ast.Imm i; dst = tmp_src; sz = X64_ast.QWord }
    ; X64_ast.Push tmp_src
    ]
  | _ as ret -> [ ret ]
;;
