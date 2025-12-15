open Ast

let fix_src = function
  | Reg _ -> assert false
  | Imm _ as src -> Reg T0, Li { src; dst = Reg T0 }
  | Stack _ as src -> Reg T0, Ld { src; dst = Reg T0 }
;;

let fix_mov_ins = function
  | Mov { src; dst } as ret ->
    (match src, dst with
     | Reg _, Reg _ -> [ ret ]
     | Reg _, _ ->
       let tmp_dst = Reg T1 in
       [ Mov { src; dst = tmp_dst }; Sd { src = tmp_dst; dst } ]
     | _, Reg _ ->
       let tmp_src, ins = fix_src src in
       [ ins; Mov { src = tmp_src; dst } ]
     | _ ->
       let tmp_src, ins = fix_src src in
       let tmp_dst = Reg T1 in
       [ ins; Mov { src = tmp_src; dst = tmp_dst }; Sd { src = tmp_dst; dst } ])
  | _ -> assert false
;;

let fix_unary_ins = function
  | Unary { uop; src; dst } as ret ->
    (match src, dst with
     | Reg _, Reg _ -> [ ret ]
     | Reg _, _ ->
       let tmp_dst = Reg T1 in
       [ Unary { uop; src; dst = tmp_dst }; Sd { src = tmp_dst; dst } ]
     | _, Reg _ ->
       let tmp_src, ins = fix_src src in
       [ ins; Unary { uop; src = tmp_src; dst } ]
     | _ ->
       let tmp_src, ins = fix_src src in
       let tmp_dst = Reg T1 in
       [ ins; Unary { uop; src = tmp_src; dst = tmp_dst }; Sd { src = tmp_dst; dst } ])
  | _ -> assert false
;;

let fix_instruction = function
  | Mov _ as mv -> fix_mov_ins mv
  | Unary _ as un -> fix_unary_ins un
  | Li _ | Ld _ | Sd _ -> assert false
  | (AllocStack _ | Ret) as ret -> [ ret ]
;;
