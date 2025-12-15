open Stdint

type identifier = Identifier of string [@@deriving show]

type const =
  | ConstInt of int32
  | ConstUInt of
      (uint32[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint32.to_string v)])
  | ConstLong of int64
  | ConstULong of
      (uint64[@printer fun fmt v -> Format.fprintf fmt "%s" (Uint64.to_string v)])
[@@deriving show]

type asm_type =
  | Byte
  | Word
  | DWord
  | QWord
[@@deriving show]
