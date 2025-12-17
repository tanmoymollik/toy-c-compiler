open Stdint
open Ast

let imm_out_of_range32 i = i > Uint64.of_int32 Int32.max_int

let fix_ins_cmp = function
  (* It is helpful to consider lhs as dst and rhs as src to better understand the semantics. *)
  | Cmp { lhs; rhs; tp = AsmDouble } as ret ->
    (match lhs with
     | Imm _ | Stack _ | Data _ ->
       let tmp_lhs = Reg Xmm15 in
       [ Mov { src = lhs; dst = tmp_lhs; tp = AsmDouble }
       ; Cmp { lhs = tmp_lhs; rhs; tp = AsmDouble }
       ]
     | _ -> [ ret ])
  | Cmp { lhs; rhs; tp } as ret ->
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
      [ Mov { src = rhs; dst = tmp_rhs; tp }
      ; Mov { src = lhs; dst = tmp_lhs; tp }
      ; Cmp { lhs = tmp_lhs; rhs = tmp_rhs; tp }
      ])
    else if invalid_rhs
    then (
      let tmp_rhs = Reg R10 in
      [ Mov { src = rhs; dst = tmp_rhs; tp }; Cmp { lhs; rhs = tmp_rhs; tp } ])
    else if invalid_lhs
    then (
      let tmp_lhs = Reg R11 in
      [ Mov { src = lhs; dst = tmp_lhs; tp }; Cmp { lhs = tmp_lhs; rhs; tp } ])
    else [ ret ]
  | _ -> assert false
;;

let fix_ins_binary = function
  | Binary { bop; src; dst; tp = AsmDouble } as ret ->
    (match bop with
     | Add | Sub | Imul | DivDouble | Xor ->
       (match dst with
        | Imm _ | Stack _ | Data _ ->
          let tmp_dst = Reg Xmm15 in
          [ Mov { src = dst; dst = tmp_dst; tp = AsmDouble }
          ; Binary { bop; src; dst = tmp_dst; tp = AsmDouble }
          ; Mov { src = tmp_dst; dst; tp = AsmDouble }
          ]
        | _ -> [ ret ])
     | _ -> [ ret ])
  | Binary { bop; src; dst; tp } as ret ->
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
         [ Mov { src; dst = tmp_src; tp }; Binary { bop; src = tmp_src; dst; tp } ])
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
         [ Mov { src; dst = tmp_src; tp }
         ; Mov { src = dst; dst = tmp_dst; tp }
         ; Binary { bop; src = tmp_src; dst = tmp_dst; tp }
         ; Mov { src = tmp_dst; dst; tp }
         ])
       else if invalid_src
       then (
         let tmp_src = Reg R10 in
         [ Mov { src; dst = tmp_src; tp }; Binary { bop; src = tmp_src; dst; tp } ])
       else if invalid_dst
       then (
         let tmp_dst = Reg R11 in
         [ Mov { src = dst; dst = tmp_dst; tp }
         ; Binary { bop; src; dst = tmp_dst; tp }
         ; Mov { src = tmp_dst; dst; tp }
         ])
       else [ ret ]
     | DivDouble -> assert false
     | Sal | Sar | Shr -> [ ret ])
  | _ -> assert false
;;

let fix_instruction = function
  | Mov { src; dst; tp } as ret ->
    (match src, dst with
     | (Stack _ | Data _), (Stack _ | Data _) ->
       let tmp_src =
         match tp with
         | AsmDouble -> Reg Xmm14
         | _ -> Reg R10
       in
       [ Mov { src; dst = tmp_src; tp }; Mov { src = tmp_src; dst; tp } ]
     | Imm i, _ when imm_out_of_range32 i && tp = DWord ->
       (* Truncate out of range imm value. *)
       [ Mov { src = Imm (Uint64.of_int32 (Uint64.to_int32 i)); dst; tp } ]
     | Imm i, (Stack _ | Data _) when imm_out_of_range32 i && tp != AsmDouble ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; tp }; Mov { src = tmp_src; dst; tp } ]
     | _ -> [ ret ])
  | Movsx { src; dst } as ret ->
    (match src with
     | Imm _ ->
       (match dst with
        | Stack _ | Data _ ->
          let tmp_src = Reg R10 in
          let tmp_dst = Reg R11 in
          [ Mov { src; dst = tmp_src; tp = DWord }
          ; Movsx { src = tmp_src; dst = tmp_dst }
          ; Mov { src = tmp_dst; dst; tp = QWord }
          ]
        | _ ->
          let tmp_src = Reg R10 in
          [ Mov { src; dst = tmp_src; tp = DWord }; Movsx { src = tmp_src; dst } ])
     | _ ->
       (match dst with
        | Stack _ | Data _ ->
          let tmp_dst = Reg R11 in
          [ Movsx { src; dst = tmp_dst }; Mov { src = tmp_dst; dst; tp = QWord } ]
        | _ -> [ ret ]))
  | MovZeroExtend { src; dst } ->
    (match dst with
     | Reg _ -> [ Mov { src; dst; tp = DWord } ]
     | Stack _ | Data _ ->
       let tmp_dst = Reg R11 in
       [ Mov { src; dst = tmp_dst; tp = DWord }; Mov { src = tmp_dst; dst; tp = QWord } ]
     | Imm _ -> assert false)
  | Cmp _ as ret -> fix_ins_cmp ret
  | Binary _ as ret -> fix_ins_binary ret
  | Idiv (src, tp) as ret ->
    (match src with
     | Imm _ ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; tp }; Idiv (tmp_src, tp) ]
     | _ -> [ ret ])
  | Div (src, tp) as ret ->
    (match src with
     | Imm _ ->
       let tmp_src = Reg R10 in
       [ Mov { src; dst = tmp_src; tp }; Div (tmp_src, tp) ]
     | _ -> [ ret ])
  | Push (Imm i) when imm_out_of_range32 i ->
    let tmp_src = Reg R10 in
    [ Mov { src = Imm i; dst = tmp_src; tp = QWord }; Push tmp_src ]
  | Cvttsd2si { src; dst; dst_tp } ->
    let tmp_dst = Reg R11 in
    [ Cvttsd2si { src; dst = tmp_dst; dst_tp }; Mov { src = tmp_dst; dst; tp = dst_tp } ]
  | Cvtsi2sd { src; dst; src_tp } as ret ->
    let invalid_src =
      match src with
      | Imm _ -> true
      | _ -> false
    in
    let invalid_dst =
      match dst with
      | Imm _ | Stack _ | Data _ -> true
      | _ -> false
    in
    if invalid_src && invalid_dst
    then (
      let tmp_src = Reg R10 in
      let tmp_dst = Reg Xmm15 in
      [ Mov { src; dst = tmp_src; tp = src_tp }
      ; Cvtsi2sd { src = tmp_src; dst = tmp_dst; src_tp }
      ; Mov { src = tmp_dst; dst; tp = AsmDouble }
      ])
    else if invalid_src
    then (
      let tmp_src = Reg R10 in
      [ Mov { src; dst = tmp_src; tp = src_tp }; Cvtsi2sd { src = tmp_src; dst; src_tp } ])
    else if invalid_dst
    then (
      let tmp_dst = Reg Xmm15 in
      [ Cvtsi2sd { src; dst = tmp_dst; src_tp }
      ; Mov { src = tmp_dst; dst; tp = AsmDouble }
      ])
    else [ ret ]
  | _ as ret -> [ ret ]
;;
