(* Auto-generated from "yuki_tree.atd" *)


type 'a node = [
    `Node2 of (string * 'a * 'a)
  | `Node3 of (string * 'a * 'a * 'a)
]

type 'a digit = [
    `One of (string * 'a)
  | `Two of (string * 'a * 'a)
  | `Three of (string * 'a * 'a * 'a)
  | `Four of (string * 'a * 'a * 'a * 'a)
]

type 'a fg = [
    `Nil
  | `Single of 'a
  | `Deep of (string * 'a digit * string option * 'a digit)
]
