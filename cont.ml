let rec factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

let rec factorial_acc n acc =
  if n = 0 then acc
  else factorial_acc (n - 1) (n * acc)

let rec factorial_cont n k =
  if n = 0 then k 1
  else factorial_cont (n - 1) (fun res -> k (n * res))

let _ = factorial 2000000

(* let cpt = ref 0
 *
 * let rec prod l =
 *   match l with
 *   | [] -> 1
 *   | 0 :: r -> 0
 *   | e :: r -> incr cpt; e * prod r
 *
 * let rec prod_cont l k =
 *   match l with
 *   | [] -> k 1
 *   | 0 :: r -> 0
 *   | e :: r -> prod_cont r (fun v -> incr cpt; k (e * v))
 *
 * type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree
 *
 * let rec map_tree (t : 'a tree) (f : 'a -> 'b) : 'b tree =
 *   match t with
 *   | Leaf -> Leaf
 *   | Node (x, left, right) -> Node (f x, map_tree left f, map_tree right f)
 *
 * let rec map_tree_cont (t : 'a tree) (f : 'a -> 'b) (k : 'b tree -> 'b tree) : 'b tree =
 *   match t with
 *   | Leaf -> k Leaf
 *   | Node (x, left, right) ->
 *     map_tree_cont left f (fun l -> map_tree_cont right f (fun r -> k (Node (f x, l, r)))) *)

(* let rec ackermann m n =
 *   if m = 0 then
 *     n + 1
 *   else
 *     if n = 0 then
 *       ackermann (m - 1) 1
 *     else
 *       ackermann m (ackermann (m - 1) n) *)

(* let rec ackermann m n k =
 *   if m = 0 then
 *     k (n + 1)
 *   else
 *     if n = 0 then
 *       ackermann (m - 1) 1 k
 *     else
 *       ackermann m (n - 1) (fun n -> ackermann (m - 1) n k)
 *
 * let res = ackermann 3 8 (fun x -> x) *)
