let tmp_var_count = ref 0

let get_var_count () =
  let c = !tmp_var_count in
  tmp_var_count := c + 1;
  c
;;
