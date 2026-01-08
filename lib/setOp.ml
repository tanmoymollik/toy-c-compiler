let equal ann1 ann2 =
  if Hashtbl.length ann1 <> Hashtbl.length ann2
  then false
  else (
    let cnt =
      Hashtbl.fold
        (fun k _ acc ->
           match Hashtbl.find_opt ann2 k with
           | Some _ -> acc + 1
           | None -> acc)
        ann1
        0
    in
    cnt = Hashtbl.length ann1)
;;

let intersect ann1 ann2 =
  let res = Hashtbl.create (Hashtbl.length ann1) in
  Hashtbl.iter
    (fun k v ->
       match Hashtbl.find_opt ann2 k with
       | Some _ -> Hashtbl.add res k v
       | None -> ())
    ann1;
  res
;;

let union ann1 ann2 =
  let res = Hashtbl.copy ann1 in
  Hashtbl.iter
    (fun k v ->
       match Hashtbl.find_opt res k with
       | Some _ -> ()
       | None -> Hashtbl.add res k v)
    ann2;
  res
;;
