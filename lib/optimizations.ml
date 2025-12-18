type level =
  | FoldConstants
  | PropagateCopies
  | UnreachableCodeElimination
  | DeadStoreElimination
  | All
  
let level_bit_pattern = function
  | FoldConstants -> 0b1
  | PropagateCopies -> 0b10
  | UnreachableCodeElimination -> 0b100
  | DeadStoreElimination -> 0b1000
  | All -> 0b1111

let encode_levels levels = List.fold_left (fun acc level -> acc lor level_bit_pattern level) 0 levels