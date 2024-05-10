open Ast

(* Abidefinitsioon, et pärast oma Graph mooduli defineerimist saaks ocamlgraph-ile ligi. *)
module Ocamlgraph = Graph

module Node = Node
module Edge = Edge

module Graph =
struct
  module G = Ocamlgraph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)
  include G
  include Ocamlgraph.Oper.P (G)
end


(** Juhtvoograaf. *)
type t = {
  entry: Node.t; (** Sisendtipp. *)
  g: Graph.t; (** Graaf ise. *)
  exit: Node.t; (** Väljundtipp. *)
}

(** Teisendab lause juhtvoograafiks. *)
let rec of_stmt (stmt: stmt): t =
  match stmt with
  | Assign (v, e) ->
    let entry = Node.fresh () in
    let exit = Node.fresh () in
    let g = Graph.add_edge_e Graph.empty (entry, Assign (v, e), exit) in
    {entry; g; exit}
  | If (c, t, f) ->
    let t_cfg = of_stmt t in
    let f_cfg = of_stmt f in
    let entry = Node.fresh () in
    let exit = Node.fresh () in
    let g = Graph.union t_cfg.g f_cfg.g in
    let g = Graph.add_edge_e g (entry, Guard (c, true), t_cfg.entry) in
    let g = Graph.add_edge_e g (entry, Guard (c, false), f_cfg.entry) in
    let g = Graph.add_edge_e g (t_cfg.exit, Nop, exit) in
    let g = Graph.add_edge_e g (f_cfg.exit, Nop, exit) in
    {entry; g; exit}
  | Nop ->
    let node = Node.fresh () in
    let g = Graph.add_vertex Graph.empty node in
    {entry = node; g; exit = node}
  | Error ->
    let entry = Node.fresh () in
    let exit = Node.fresh () in
    let g = Graph.add_edge_e Graph.empty (entry, Error, exit) in
    {entry; g; exit}
  | Seq (a, b) ->
    let a_cfg = of_stmt a in
    let b_cfg = of_stmt b in
    let g = Graph.union a_cfg.g b_cfg.g in
    let g = Graph.add_edge_e g (a_cfg.exit, Nop, b_cfg.entry) in
    {entry = a_cfg.entry; g; exit = b_cfg.exit}
  | While (c, b) ->
    let b_cfg = of_stmt b in
    let entry = Node.fresh () in
    let exit = Node.fresh () in
    let g = b_cfg.g in
    let g = Graph.add_edge_e g (entry, Guard (c, true), b_cfg.entry) in
    let g = Graph.add_edge_e g (entry, Guard (c, false), exit) in
    let g = Graph.add_edge_e g (b_cfg.exit, Nop, entry) in
    {entry; g; exit}


(** Abifunktsioonid. *)

(** Tagastab tipust väljuvad servad ja vastavad sihttipud. *)
let succ (cfg: t) (node: Node.t): (Edge.t * Node.t) list =
  Graph.succ_e cfg.g node
  |> List.map (fun (_, edge, node') ->
      (edge, node')
    )

(** Tagastab tippu sisenevad servad ja vastavad lähtetipud. *)
let pred (cfg: t) (node: Node.t): (Edge.t * Node.t) list =
  Graph.pred_e cfg.g node
  |> List.map (fun (node', edge, _) ->
      (edge, node')
    )

(** Kas tipp on Error-i kohal? *)
let is_error (cfg: t) (node: Node.t): bool =
  succ cfg node
  |> List.exists (fun (edge, _) ->
      edge = Edge.Error
    )

(** Tagastab kõik tipud. *)
let nodes (cfg: t): Node.t list =
  Graph.fold_vertex List.cons cfg.g []


