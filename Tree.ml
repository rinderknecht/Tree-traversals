(* Example of preorder and postorder traversals of general trees (here
   an Abstract-Syntax Tree (AST), used in compilers) *)

(* Utilities *)

let identity x = x
let (<@) f g x = f (g x)

(* LIBRARY-SIDE *)

(* Tree to be iterated (and AST) *)

module AST =
  struct
    type declarations = decl list

    and decl =
      Fun  of string * expr
    | Type of string * type_expr
    | Expr of expr * type_expr option

    and expr =
      Call  of string * expr list
    | Value
    | Block of declarations

    and type_expr =
      Int
    | String
    | Arrow of type_expr * type_expr
  end

(* Forest based on the AST *)

(* Given a node of the tree (AST), the functions in module [Forest]
   return the sub-forest whose roots are nodes of interest, that is,
   nodes that are used to update the accumulator of the fold, either
   in preorder or postorder. *)

module Forest =
  struct
    (* Nodes of interest from the AST. *)

    (* The functions of type [t -> t] add to their argument the
       subforest below the node. This enables the caller of the
       functions below to complete the forest to iterate over. *)

    type tree = [
      `Expr of AST.expr      * (t -> t)
    | `Type of AST.type_expr * (t -> t)
    ]

    and t = tree list

    type forest = t

    (* Extracting the greatest sub-forest from a tree *)

    (* Note the use of [List.fold_right]. Beyond preserving the order
       of the sub-forests, it enables the eta-conversion of the
       right-hand sides. *)

    let rec of_declarations (node : AST.decl list) =
      List.fold_right of_decl node

    and of_decl (node : AST.decl) =
      let open AST in
      match node with
        Fun  (_, e)      -> of_expr e
      | Type (_, t)      -> of_type_expr t
      | Expr (e, None)   -> of_expr e
      | Expr (e, Some t) -> of_expr e <@ of_type_expr t

    and of_expr (node : AST.expr) =
      let push_children =
        let open AST in
        match node with
          Call (_, args) -> List.fold_right of_expr args
        | Value          -> identity
        | Block decls    -> of_declarations decls
      in List.cons (`Expr (node, push_children))

    and of_type_expr (node : AST.type_expr) =
      let push_children =
        let open AST in
        match node with
          Int            -> identity
        | String         -> identity
        | Arrow (t1, t2) -> of_type_expr t1 <@ of_type_expr t2
      in List.cons (`Type (node, push_children))

    (* Exported *)

    let of_declarations node = of_declarations node []
  end

(* CLIENT-SIDE *)

(* Updating the accumulator and the remaining forest to iterate *)

module Acc =
  struct
    (* As an example, we gather the names of the function being called
       in preorder, and the number of integer types. *)

    type t = {fun_names : string list; int_types : int}

    type acc = t

    (* Updating the nodes of interest *)

    let update_expr (node : AST.expr) (acc : t) : t =
      let open AST in
      match node with
        Call (name, _) -> {acc with fun_names = name :: acc.fun_names}
      | Value          -> acc
      | Block _        -> acc

    let update_type (node : AST.type_expr) (acc : t) : t =
      let open AST in
      match node with
        Int     -> {acc with int_types = acc.int_types + 1}
      | String  -> acc
      | Arrow _ -> acc
  end

(* Folding in preorder and postorder over a forest *)

let update (tree : Forest.tree) (forest : Forest.t) =
  match tree with
    `Expr (e, push_children) -> Acc.update_expr e, push_children forest
  | `Type (t, push_children) -> Acc.update_type t, push_children forest

let rec preorder acc = function
  [] -> acc
| tree :: forest ->
    let upd, forest = update tree forest
    in preorder (upd acc) forest

let rec postorder acc = function
  [] -> acc
| tree :: forest ->
    let upd, forest = update tree forest
    in upd @@ postorder acc forest

(* Example *)

open AST

let tree = [
  Fun ("x", Block [
              Type ("t", Int);
              Fun ("y", Call ("a", [Value]))]);
  Type ("u", String);
  Expr (Call ("b", []),
        Some (Arrow (Int,
                     Arrow (String, Int))));
  Expr (Call ("c", [Call ("d", [Value]);
                    Call ("e", [Value])]),
        None)]

let init   = Acc.{fun_names = []; int_types = 0}
let forest = Forest.of_declarations tree

let _ : Acc.t = preorder  init forest
let _ : Acc.t = postorder init forest

(*
# #use "Tree.ml";;
[...]
- : Acc.t = {Acc.fun_names = ["e"; "d"; "c"; "b"; "a"]; int_types = 3}
- : Acc.t = {Acc.fun_names = ["a"; "b"; "c"; "d"; "e"]; int_types = 3}
*)
