open Core_kernel
open QCheck

(** Generic lattice tests *)

module type LATTICE_TOPLESS = sig
  val name : string

  type t

  val leq : t -> t -> bool

  val join : t -> t -> t

  val meet : t -> t -> t

  val bottom : t

  (*  val top       : t *)
  val equal : t -> t -> bool

  val to_string : t -> string

  val arb_elem : t arbitrary

  val equiv_pair : (t * t) arbitrary

  val arb_elem_le : t -> t arbitrary
end

module type LATTICE = sig
  include LATTICE_TOPLESS

  val top : t
end

module GenericTests (L : LATTICE_TOPLESS) = struct
  (* Helpers for generating pairs and triples *)
  let arb_pair = pair L.arb_elem L.arb_elem

  let arb_triple = triple L.arb_elem L.arb_elem L.arb_elem

  (* Helpers for pretty printing pairs and triples *)
  let pp_pair = Print.pair L.to_string L.to_string

  let pp_triple = Print.triple L.to_string L.to_string L.to_string

  let ord_pair =
    let gen =
      Gen.(L.arb_elem.gen >>= fun e -> pair (L.arb_elem_le e).gen (return e))
    in
    make gen ~print:pp_pair

  let ord_triple =
    let gen =
      Gen.(
        L.arb_elem.gen >>= fun e ->
        (L.arb_elem_le e).gen >>= fun e' ->
        (L.arb_elem_le e').gen >>= fun e'' -> return (e'', e', e))
    in
    make gen ~print:pp_triple

  let small a = String.length (L.to_string a)

  let small_pair p = String.length (pp_pair p)

  let small_triple t = String.length (pp_triple t)

  (* Generic lattice property tests *)
  let leq_refl =
    (* forall a. a <= a *)
    Test.make ~count:1000 ~max_fail:1 ~name:("leq reflexive in " ^ L.name)
      ~small L.arb_elem (fun a -> L.leq a a)

  let leq_trans =
    (* forall a,b,c. a <= b /\ b <= c  =>  a <= c *)
    Test.make ~count:1000 ~max_fail:1 ~name:("leq transitive in " ^ L.name)
      ~small:small_triple ord_triple (* arb_triple *) (fun (a, b, c) ->
        assume (L.leq a b);
        assume (L.leq b c);
        L.leq a c)

  let leq_antisym =
    (* forall a,b. a <= b /\ b <= a  =>  a = b *)
    Test.make ~count:1000 ~max_fail:1 ~name:("leq anti symmetric in " ^ L.name)
      ~small:small_pair L.equiv_pair
      (* Alternatively: Arbitrary.(choose [L.equiv_pair; ord_pair; ord_pair >>= fun (a,b) -> return (b,a)]) *)
      (fun (a, b) ->
        assume (L.leq a b);
        assume (L.leq b a);
        L.equal a b)

  (*let top_is_upperbound = (* forall a. a <= top *)
    Test.make ~count:1000 ~max_fail:1 ~name:("top is upper bound in " ^ L.name)
            ~small:(fun a -> String.length (pp_pair a))
      L.arb_elem
      (fun a -> L.(leq a top)) *)

  let bot_is_lowerbound =
    (* forall a. bottom <= a *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("bottom is lower bound in " ^ L.name) ~small L.arb_elem (fun a ->
        L.(leq bottom a))

  let join_comm =
    (* forall a,b. a \/ b = b \/ a *)
    Test.make ~count:1000 ~max_fail:1 ~name:("join commutative in " ^ L.name)
      ~small:small_pair arb_pair (fun (a, b) -> L.(equal (join a b) (join b a)))

  let join_assoc =
    (* forall a,b,c. (a \/ b) \/ c = a \/ (b \/ c) *)
    Test.make ~count:1000 ~max_fail:1 ~name:("join associative in " ^ L.name)
      ~small:small_triple arb_triple (fun (a, b, c) ->
        L.(equal (join (join a b) c) (join a (join b c))))

  let join_idempotent =
    (* forall a. a \/ a = a *)
    Test.make ~count:1000 ~max_fail:1 ~name:("join idempotent in " ^ L.name)
      ~small L.arb_elem (fun a -> L.(equal (join a a) a))

  let meet_comm =
    (* forall a,b. a /\ b = b /\ a *)
    Test.make ~count:1000 ~max_fail:1 ~name:("meet commutative in " ^ L.name)
      ~small:small_pair arb_pair (fun (a, b) -> L.(equal (meet a b) (meet b a)))

  let meet_assoc =
    (* forall a,b,c. (a /\ b) /\ c = a /\ (b /\ c) *)
    Test.make ~count:1000 ~max_fail:1 ~name:("meet associative in " ^ L.name)
      ~small:small_triple arb_triple (fun (a, b, c) ->
        L.(equal (meet (meet a b) c) (meet a (meet b c))))

  let meet_idempotent =
    (* forall a. a /\ a = a *)
    Test.make ~count:1000 ~max_fail:1 ~name:("meet idempotent in " ^ L.name)
      ~small L.arb_elem (fun a -> L.(equal (meet a a) a))

  let join_meet_absorption =
    (* forall a,b. a \/ (a /\ b) = a *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("join meet absorbtion in " ^ L.name) ~small:small_pair arb_pair
      (fun (a, b) -> L.(equal (join a (meet a b)) a))

  let meet_join_absorption =
    (* forall a,b. a /\ (a \/ b) = a *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("meet join absorbtion in " ^ L.name) ~small:small_pair arb_pair
      (fun (a, b) -> L.(equal (meet a (join a b)) a))

  let leq_compat_join =
    (* forall a,b. a < b  ==>  a \/ b = b  *)
    Test.make ~count:1000 ~max_fail:1 ~name:("leq compatible join in " ^ L.name)
      ~small:small_pair ord_pair (*arb_pair*) (fun (a, b) ->
        assume (L.leq a b);
        L.(equal (join a b) b))

  let join_compat_leq =
    (* forall a,b. a \/ b = b  ==> a < b  *)
    Test.make ~count:1000 ~max_fail:1 ~name:("join compatible leq in " ^ L.name)
      ~small:small_pair ord_pair (*arb_pair*) (fun (a, b) ->
        assume L.(equal (join a b) b);
        L.leq a b)

  let join_compat_meet =
    (* forall a,b. a \/ b = b  ==>  a /\ b  = a  *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("join compatible meet in " ^ L.name) ~small:small_pair ord_pair
      (*arb_pair*) (fun (a, b) ->
        assume L.(equal (join a b) b);
        L.(equal (meet a b) a))

  let meet_compat_join =
    (* forall a,b. a /\ b  = a  ==>  a \/ b = b    *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("meet compatible join in " ^ L.name) ~small:small_pair ord_pair
      (*arb_pair*) (fun (a, b) ->
        assume L.(equal (meet a b) a);
        L.(equal (join a b) b))

  let meet_compat_leq =
    (* forall a,b. a /\ b  = a  ==>  a <= b  *)
    Test.make ~count:1000 ~max_fail:1 ~name:("meet compatible leq in " ^ L.name)
      ~small:small_pair ord_pair (*arb_pair*) (fun (a, b) ->
        assume L.(equal (meet a b) a);
        L.leq a b)

  let leq_compat_meet =
    (* forall a,b. a <= b  ==>  a /\ b  = a  *)
    Test.make ~count:1000 ~max_fail:1 ~name:("leq compatible meet in " ^ L.name)
      ~small:small_pair ord_pair (*arb_pair*) (fun (a, b) ->
        assume (L.leq a b);
        L.(equal (meet a b) a))

  (* Consistency check: generated ordered pairs are in fact ordered *)
  let check_ordering =
    Test.make ~count:1000 ~max_fail:1
      ~name:("generated ordered pairs consistent in " ^ L.name)
      ~small:small_pair ord_pair (fun (a, b) -> L.leq a b)

  let pp_pair = Print.pair L.to_string L.to_string

  let ord_pair =
    let gen =
      Gen.(L.arb_elem.gen >>= fun v -> pair (L.arb_elem_le v).gen (return v))
    in
    make gen ~print:pp_pair

  let suite =
    [
      leq_refl;
      leq_trans;
      leq_antisym;
      (*top_is_upperbound;*)
      bot_is_lowerbound;
      join_comm;
      join_assoc;
      join_idempotent;
      meet_comm;
      meet_assoc;
      meet_idempotent;
      join_meet_absorption;
      meet_join_absorption;
      (* compatibility *)
      leq_compat_join;
      join_compat_leq;
      join_compat_meet;
      meet_compat_join;
      meet_compat_leq;
      leq_compat_meet;
      check_ordering;
    ]
end

module GenericTopTests (L : LATTICE) = struct
  let top_is_upperbound =
    (* forall a. a <= top *)
    Test.make ~count:1000 ~max_fail:1
      ~name:("top is upper bound in " ^ L.name)
      ~small:(fun a -> String.length (L.to_string a))
      L.arb_elem
      (fun a -> L.(leq a top))

  let suite = [ top_is_upperbound ]
end

(** EDSL for lattice operation tests *)

let ord_pair (type a) (module L : LATTICE_TOPLESS with type t = a) =
  let gen =
    Gen.(L.arb_elem.gen >>= fun e -> pair (L.arb_elem_le e).gen (return e))
  in
  make gen

module type ARB_ARG = sig
  (*  val name  : string *)
  type t

  val arb_elem : t arbitrary

  val to_string : t -> string
end

module MkArbListArg (A : ARB_ARG) = struct
  type t = A.t list

  let arb_elem = list_of_size (Gen.int_bound 20) A.arb_elem

  let to_string = Print.list A.to_string
end

let op_monotone (type a b) (module PL : LATTICE_TOPLESS with type t = a)
    (module RL : LATTICE_TOPLESS with type t = b) k =
  let ord_pair =
    let gen =
      Gen.(PL.arb_elem.gen >>= fun e -> pair (PL.arb_elem_le e).gen (return e))
    in
    make gen
  in
  k
    ( Print.pair PL.to_string PL.to_string,
      ord_pair,
      (fun op (v, v') ->
        assume (PL.leq v v');
        RL.leq (op v) (op v')),
      "monotone",
      1 )

let op_invariant (type a b) (module PL : LATTICE_TOPLESS with type t = a)
    (module RL : LATTICE_TOPLESS with type t = b) k =
  k
    ( Print.pair PL.to_string PL.to_string,
      PL.equiv_pair,
      (fun op (v, v') ->
        assume (PL.equal v v');
        RL.equal (op v) (op v')),
      "invariant",
      1 )

let op_strict (type a b) (module PL : LATTICE_TOPLESS with type t = a)
    (module RL : LATTICE_TOPLESS with type t = b) k =
  k
    ( PL.to_string,
      always PL.bottom,
      (fun op bottom ->
        assume (PL.equal bottom PL.bottom);
        RL.equal (op bottom) RL.bottom),
      "strict",
      1 )

let op_distributive (type a b) (module PL : LATTICE_TOPLESS with type t = a)
    (module RL : LATTICE_TOPLESS with type t = b) k =
  let arb_pair = pair PL.arb_elem PL.arb_elem in
  k
    ( Print.pair PL.to_string PL.to_string,
      arb_pair,
      (fun op (v, v') -> RL.equal (op (PL.join v v')) (RL.join (op v) (op v'))),
      "distributive",
      1 )

let pw_left (type a) (module PL : ARB_ARG with type t = a) op_prop m1 m2 k =
  op_prop m1 m2 (fun (subpp, subgen, prop, pname, leftargs) ->
      k
        ( Print.pair PL.to_string subpp,
          pair PL.arb_elem subgen,
          (fun op (a, b) -> prop (op a) b),
          pname,
          leftargs + 1 ))

let pw_right (type a) (module PL : ARB_ARG with type t = a) op_prop m1 m2 k =
  op_prop m1 m2 (fun (subpp, subgen, prop, pname, leftargs) ->
      k
        ( Print.pair subpp PL.to_string,
          pair subgen PL.arb_elem,
          (fun op (p, st) -> prop (fun v -> op v st) p),
          pname,
          leftargs ))

let ( ---> ) (type e e') a (b : (module LATTICE_TOPLESS with type t = e)) k =
  a (fun (l, optranl, r, optranr, prop) ->
      let module R = (val r : LATTICE_TOPLESS with type t = e') in
      (* manual "upcast" *)
      let r = (module R : ARB_ARG with type t = e') in
      (* the module type *)
      k
        ( l,
          (fun prop -> optranl (pw_left r prop)),
          b,
          (fun prop -> optranr (pw_right r prop)),
          prop ))

let ( -<-> ) (type e) a (b : (module LATTICE_TOPLESS with type t = e)) k =
  a (fun (_l, optranl, r, _, _) ->
      k (r, (fun prop -> prop), b, optranl, op_monotone))

let ( -$-> ) (type e) a (b : (module LATTICE_TOPLESS with type t = e)) k =
  a (fun (_l, optranl, r, _, _) ->
      k (r, (fun prop -> prop), b, optranl, op_strict))

let ( -~-> ) (type e) a (b : (module LATTICE_TOPLESS with type t = e)) k =
  a (fun (_l, optranl, r, _, _) ->
      k (r, (fun prop -> prop), b, optranl, op_invariant))

let ( -%-> ) (type e) a (b : (module LATTICE_TOPLESS with type t = e)) k =
  a (fun (_l, optranl, r, _, _) ->
      k (r, (fun prop -> prop), b, optranl, op_distributive))

let testsig (type e) (i : (module LATTICE_TOPLESS with type t = e)) k =
  k (i, (fun prop -> prop), i, (fun prop -> prop), fun _ -> assert false)

let finalize opsig (opname, op) =
  opsig (fun (pp, gen, prop, pname, leftargs) ->
      Test.make ~count:1000 ~max_fail:1
        ~small:(fun a -> String.length (pp a))
        ~name:(Printf.sprintf "'%s %s in argument %i'" opname pname leftargs)
        gen (prop op))

let ( =:: ) a (b, c) = finalize a (b, c)

let for_op (l, _, r, optrans, prop) (opname, op) =
  finalize (optrans prop l r) (opname, op)

let ( =: ) a (b, c) = a for_op (b, c)

(** A number of reusable lattices *)

(* Note: the following module represents the Boolean lattice:   *)
(*       {true,false} under reverse implication ordering,       *)
(*        bottom = true <== true <== false <== false = top         *)
module Bool = struct
  let name = "Boolean lattice"

  type t = bool

  let leq a b = if a then true else not b

  let join = ( && )

  let meet = ( || )

  let bottom = true

  let top = false

  let equal = equal_bool

  let to_string = string_of_bool

  let pprint = Format.printf "%b"

  (* The below ones are generic *)
  let arb_elem = bool

  let equiv_pair = map (fun a -> (a, a)) arb_elem

  let arb_elem_le e = if equal e top then arb_elem else always bottom
end

(* Note: the following module represents the dual Boolean lattice:   *)
(*       {true,false} under implication ordering,                    *)
(*        bottom = false <== false <== true <== true = top              *)
module DBool = struct
  let name = "Dual Boolean lattice"

  type t = bool

  let leq a b = if a then b else true

  let join = ( || )

  let meet = ( && )

  let bottom = false

  let top = true

  let equal = equal_bool

  let to_string = string_of_bool

  let pprint = Format.printf "%b"

  (* The below ones are generic *)
  let arb_elem = bool

  let equiv_pair = map (fun a -> (a, a)) arb_elem

  let arb_elem_le e = if equal e top then arb_elem else always bottom
end

module GenBoolTests = GenericTests (Bool)
module GenBoolTopTests = GenericTopTests (Bool)
module GenDBoolTests = GenericTests (DBool)
module GenDBoolTopTests = GenericTopTests (DBool)

module MkPairLattice (A : LATTICE_TOPLESS) (B : LATTICE_TOPLESS) = struct
  let name = "(" ^ A.name ^ " * " ^ B.name ^ ") pair lattice"

  type t = A.t * B.t

  let leq p p' = A.leq (fst p) (fst p') && B.leq (snd p) (snd p')

  let join p p' = (A.join (fst p) (fst p'), B.join (snd p) (snd p'))

  let meet p p' = (A.meet (fst p) (fst p'), B.meet (snd p) (snd p'))

  let bottom = (A.bottom, B.bottom)

  let equal p p' = A.equal (fst p) (fst p') && B.equal (snd p) (snd p')

  let to_string = Print.pair A.to_string B.to_string

  let pprint p = Format.printf "%s" (to_string p)

  let arb_elem = pair A.arb_elem B.arb_elem

  let equiv_pair = map (fun a -> (a, a)) arb_elem

  let arb_elem_le p = pair (A.arb_elem_le (fst p)) (B.arb_elem_le (snd p))
end

module MkListLattice (A : LATTICE_TOPLESS) = struct
  let name = "(" ^ A.name ^ ") list lattice"

  type t = A.t list

  let rec leq vs us =
    match (vs, us) with
    | [], _ -> true
    | _, [] -> false
    | v :: vs, u :: us -> A.leq v u && leq vs us

  let rec join vs us =
    match (vs, us) with
    | [], _ -> us
    | _, [] -> vs
    | v :: vs, u :: us -> A.join v u :: join vs us

  let rec meet vs us =
    match (vs, us) with
    | [], _ -> []
    | _, [] -> []
    | v :: vs, u :: us -> A.meet v u :: meet vs us

  let bottom = []

  let rec equal vs us =
    match (vs, us) with
    | [], [] -> true
    | v :: vs, u :: us -> A.equal v u && equal vs us
    | _, _ -> false

  let to_string = Print.list A.to_string

  let pprint vs = Format.printf "%s" (to_string vs)

  let arb_elem = list A.arb_elem

  let equiv_pair = map (fun v -> (v, v)) arb_elem

  let arb_elem_le vs =
    map
      (fun i ->
        let smaller_vs, _ = List.split_n vs i in
        List.fold_right smaller_vs
          ~f:(fun v acc_gen ->
            let arb = A.arb_elem_le v in
            arb :: acc_gen)
          ~init:[])
      (int_bound (1 + List.length vs))
end
