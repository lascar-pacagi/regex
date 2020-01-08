(** [explode s] returns the list of the characters of the string [s].
    For example, [explode "hello"] is ['h'; 'e'; 'l'; 'l'; 'o']. *)
let explode s =
  List.init (String.length s) (fun i -> String.get s i)

(** [implode l] returns a string from the list of characters [l].
    For example, [implode ['h'; 'e'; 'l'; 'l'; 'o']] is ["hello"]. *)
let implode l =
  let res = Bytes.create (List.length l) in
  let rec imp i = function
    | [] -> ()
    | c :: l -> Bytes.set res i c; imp (i + 1) l in
  imp 0 l;
  Bytes.unsafe_to_string res

(** [i -- j] creates a list of the integers [i, i+1, ..., j-1, j].
    For example, [1 -- 5] is [1;2;3;4;5]. *)
let (--) i j =
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j []

(** [CSet] is a character set module. *)
module CSet = Set.Make(Char)

(** [ascii_set] is a character set containing all the ascii characters. *)
let ascii_set =
  List.init 128 Char.chr
  |> CSet.of_list

module type RegularExpression = sig
  type regex =
    | CharSet of CSet.t              (** [CharSet {c1, c2, ..., cn}] represents the regex (c1 | c2 | ... | cn),
                                         where each ci is an ascii character. *)
    | Concatenation of regex * regex (** [Concatenation (r1, r2)] represents the regex (r1r2). *)
    | Union of regex * regex         (** [Union (r1, r2)] represents the regex (r1 | r2). *)
    | ZeroOrOne of regex             (** [ZeroOrOne r] represents the regex (r | epsilon). *)
    | ZeroOrMore of regex            (** [ZeroOrMore r] represents the regex ( r* ). *)
    | OneOrMore of regex             (** [OneOrMore r] represents the regex ( rr* ). *)

  (** [charset set] is a constructor and returns [CharSet set]. *)
  val charset : CSet.t -> regex

  (** [concat r1 r2] is a constructor and returns [Concatenation (r1, r2)]. *)
  val concat  : regex -> regex -> regex

  (** [union r1 r2] is a constructor and returns [Union (r1, r2)]. *)
  val union   : regex -> regex -> regex

  (** [zero_or_one r] is a constructor and returns [ZeroOrOne r]. *)
  val zero_or_one  : regex -> regex

  (** [zero_or_more r] is a constructor and returns [ZeroOrMore r]. *)
  val zero_or_more : regex -> regex

  (** [one_or_more r] is a constructor and returns [OneOrMore r]. *)
  val one_or_more  : regex -> regex

  (** [regex_from_string s] returns a [regex] that represents the regular expression [s].
      In the following a character c will be denoted by 'c'. We suppose that we do not need to escape
      a character between simple quotes to avoid confusion. For example, we will write '\' and not '\\'.
      The different operators in the regular expression are
      - '?' represents zero or one.
      - '*' represents zero or more.
      - '+' represents one or more.
      - '.' represents any character.
      - '(' and ')' are used to group regular expressions.
      - '[' and ']' are explained below.
      - the special characters ('?', '*', '+', '.', '(', ')', '[', ']') can be escaped with the character '\'.
         For example, '\' followed by '*' represents the character star and not the operator zero or more. '\' can be escaped too
         with '\' followed by '\'. Note that in a normal string, in OCaml, to obtain '\' followed by '*', you need to type "\\*".
         You can also use the quoted string {|\*|} in OCaml.
         To obtain '\' followed by '\' you need to type "\\\\" or use the quoted string {|\\|}.
      - "[c1-c2]" represents the range of ascii characters between c1 and c2. For example, "[a-zA-Z]"
         is the set of lower case and upper case letters.
      - "[c1c2...cn]" represents the ascii characters c1, c2, ..., cn. For example, "[cofe]" represents
         the four characters 'c', 'o', 'f' and 'e'.
      - "[^c1-c2]" represents all the ascii characters except for those between c1 and c2. For example,
         "[^a-z]" represents all the ascii characters except for the lower case letters.
      - "[^c1c2...cn]" represents all the ascii characters except for c1, c2, ..., cn. For example, "[^/*]" represents
         all the ascii characters except for the two characters '/' and '*'.
      - Inside "[]" we don't escape the special characters except for ']' (but if you want to type '\' you still need to write "[\\]").
   *)
  val regex_from_string : string -> regex

  (** [print fmt re] prints the regular expression [re] with the formatter [fmt], showing the content of the character sets. *)
  val print : Format.formatter -> regex -> unit
end

module RE : RegularExpression = struct
  type regex =
    | CharSet of CSet.t
    | Concatenation of regex * regex
    | Union of regex * regex
    | ZeroOrOne of regex
    | ZeroOrMore of regex
    | OneOrMore of regex

  let charset set = CharSet set

  let concat re1 re2 = Concatenation (re1, re2)

  let union re1 re2 = Union (re1, re2)

  let zero_or_one re = ZeroOrOne re

  let zero_or_more re = ZeroOrMore re

  let one_or_more re = OneOrMore re

  let regex_from_string s =
    let rec re l =
      let e, l = re1 l in
      match l with
      | '|' :: r ->
         let e', l = re r in
         Union (e, e'), l

      | _ ->
         e, l
    and re1 l =
      let e, l = re2 l in
      let e, l =
        let rec re1' e l =
          match l with
          | '?' :: r ->
             re1' (ZeroOrOne e) r

          | '*' :: r ->
             re1' (ZeroOrMore e) r

          | '+' :: r ->
             re1' (OneOrMore e) r

          | _ ->
             e, l
        in
        re1' e l
      in
      match l with
      | c :: _ when c <> ')' && c <> '|' ->
         let e', l = re1 l in
         Concatenation (e, e'), l

      | _ ->
         e, l
    and re2 l =
      match l with
      | '(' :: r ->
         begin
           let e, l = re r in
           match l with
           | ')' :: r ->
              e, r
           | _ ->
              failwith "re2: ')' expected in re2"
         end

      | '.' :: r ->
         CharSet ascii_set, r

      | '[' :: '^' :: r ->
         let set, r = char_set r CSet.empty in
         CharSet (CSet.diff ascii_set set), r

      | '[' :: r ->
         let set, r = char_set r CSet.empty in
         CharSet set, r

      | '\\' :: ('.' | '*' | '+' | '?' | '\\' | '(' | ')' | '[' | ']' as c) :: r ->
         CharSet (CSet.singleton c), r

      | c :: r ->
         CharSet (CSet.singleton c), r

      | [] ->
         failwith "re2: character expected"

    and char_set l acc =
      match l with
      | ']' :: r ->
         if CSet.is_empty acc then
           failwith "char_set: empty";
         acc, r

      | c1 :: '-' :: c2 :: r ->
         chars_between c1 c2
         |> CSet.union acc
         |> char_set r

      | '\\' :: (']' as c) :: r
      | c :: r ->
         CSet.add c acc
         |> char_set r

      | [] ->
         failwith "char_set: character expected"
    and chars_between c1 c2 =
      (Char.code c1) -- (Char.code c2)
      |> List.map Char.chr
      |> CSet.of_list
    in
    let e, l = re (explode s) in
    if l <> [] then
      failwith "regexp_from_string : not a valid regexp"
    else
      e

  let print ppf re =
    let range_from_cset cset =
      let rec aux (a, b) acc = function
        | [] ->
           (a, b) :: acc

        | c :: r ->
           if c = b + 1 then
             aux (a, c) acc r
           else
             aux (c, c) ((a, b) :: acc) r
      in
      let elts =
        CSet.elements cset
        |> List.map Char.code
      in
      let first_code = List.hd elts in
      aux (first_code, first_code) [] (List.tl elts)
      |> List.map (fun (a, b) -> (Char.chr a, Char.chr b))
    in
    let char_to_string_escaped c =
        String.make 1 c
        |> String.escaped
    in
    let open Format in
    let rec print_range_list ppf = function
      | [] ->
         ()

      | (a, b) :: r ->
         (if a = b then
            fprintf ppf "'%s'@,%s%a" (char_to_string_escaped a)
          else
            fprintf ppf "['%s'-'%s']@,%s%a"
              (char_to_string_escaped a)
              (char_to_string_escaped b))
           (if r = [] then "" else " ")
           print_range_list r
    in
    let rec print_regexp ppf = function
      | CharSet set ->
         fprintf ppf "@[<1>CharSet(%a)@]"
           print_range_list (range_from_cset set)

      | Concatenation (re1, re2) ->
         fprintf ppf "@[<1>Concatenation (%a,@ %a)@]"
           print_regexp re1
           print_regexp re2

      | Union (re1, re2) ->
         fprintf ppf "@[<1>Union (%a,@ %a)@]"
           print_regexp re1
           print_regexp re2

      | ZeroOrOne re ->
         fprintf ppf "@[<1>ZeroOrOne (%a)@]"
           print_regexp re

      | ZeroOrMore re ->
         fprintf ppf "@[<1>ZeroOrMore(%a)@]"
           print_regexp re

      | OneOrMore re ->
         fprintf ppf "@[<1>OneOrMore (%a)@]"
           print_regexp re
    in
    fprintf ppf "%a@."
      print_regexp re

end

module type Matching = sig
  type t

  (** [init re] returns the type [t] produced by the matching module from [re].
      For example, the type [t] could be the NFA representation of this regular expression. *)
  val init : RE.regex -> t

  (** [full_match t s] returns [true] if [t] can match the string [s] and [false] otherwise. *)
  val full_match : t -> string -> bool
end

module Backtracking : Matching = struct
  type t = RE.regex

  let init re = re

  let full_match t s =
    let rec full_match t l k =
      let open RE in
      match t with
      | CharSet set ->
         begin
           match l with
           | c' :: r when CSet.mem c' set -> k r
           | _ -> false
         end

      | Concatenation (t1, t2) ->
         full_match t1 l (fun l -> full_match t2 l k)

      | Union (t1, t2) ->
         full_match t1 l k
         || full_match t2 l k

      | ZeroOrOne t ->
         full_match t l k
         || k l

      | ZeroOrMore t1 ->
         full_match t1 l (fun l' -> l <> l' && full_match t l' k)
         || k l

      | OneOrMore t1 ->
         full_match t1 l (fun l -> full_match (ZeroOrMore t1) l k)
    in
    full_match
      t
      (explode s)
      (fun l -> l = [])

end

module NFA : Matching = struct
  type t = (int * state) ref
  and state =
    | Split of t * t
    | CharSet of CSet.t * t
    | Match

  let make_t =
    let counter = ref 1 in
    (fun s ->
      if s = Match then
        ref (0, s)
      else
        begin
          let i = !counter in
          incr counter;
          ref (i, s)
        end)

  let init re =
    let rec init re next =
      match re with
      | RE.CharSet set ->
         make_t (CharSet (set, next))

      | RE.Concatenation (re1, re2) ->
         init re2 next
         |> init re1

      | RE.Union (re1, re2) ->
         make_t (Split (init re1 next, init re2 next))

      | RE.ZeroOrOne re ->
         make_t (Split (init re next, next))

      | RE.ZeroOrMore re ->
         let t = make_t Match in
         let res = make_t (Split (init re t, next)) in
         t := !res;
         res

      | RE.OneOrMore re ->
         let t = make_t Match in
         let res = init re t in
         t := !(make_t (Split (res, next)));
         res
    in
    init re (make_t Match)

  let full_match t s =
    let module S =
      Set.Make(
          struct
            type t = (int * state) ref
            let compare { contents = (i1, _) } { contents = (i2, _) } =
              compare i1 i2
          end)
    in
    let rec follow ({ contents = (_, s) } as t) next =
      if S.mem t next then
        next
      else
        let next = S.add t next in
        match s with
        | Split (t1, t2) ->
           follow t1 next
           |> follow t2

        | _ ->
           next
    in
    let step curr c =
      S.fold
        (fun { contents = (_, s) } next ->
          match s with
          | CharSet (set, t) when CSet.mem c set ->
             follow t next

          | _ ->
             next
        ) curr S.empty
    in
    let rec full_match l curr =
      match l with
      | [] ->
         S.mem (make_t Match) curr

      | c :: r ->
         step curr c
         |> full_match r
    in
    full_match (explode s) (follow t S.empty)

end

module DFA : Matching = struct
  type t1 = (int * state) ref
  and state =
    | Split of t1 * t1
    | CharSet of CSet.t * t1
    | Match

  module S =
    Set.Make(
        struct
          type t = t1
          let compare { contents = (i1, _) } { contents = (i2, _) } =
            compare i1 i2
        end)

  module Memo =
    Map.Make(
        struct
          type t = S.t * char
          let compare (s1, c1) (s2, c2) =
            let res = compare c1 c2 in
            if res = 0 then
              S.compare s1 s2
            else
              res
        end)

  type t = S.t Memo.t ref * t1

  let make_t1 =
    let counter = ref 1 in
    (fun s ->
      if s = Match then
        ref (0, s)
      else
        begin
          let i = !counter in
          incr counter;
          ref (i, s)
        end)

  let init re =
    let rec init re next =
      match re with
      | RE.CharSet set ->
         make_t1 (CharSet (set, next))

      | RE.Concatenation (re1, re2) ->
         init re2 next
         |> init re1

      | RE.Union (re1, re2) ->
         make_t1 (Split (init re1 next, init re2 next))

      | RE.ZeroOrOne re ->
         make_t1 (Split (init re next, next))

      | RE.ZeroOrMore re ->
         let t1 = make_t1 Match in
         let res = make_t1 (Split (init re t1, next)) in
         t1 := !res;
         res

      | RE.OneOrMore re ->
         let t1 = make_t1 Match in
         let res = init re t1 in
         t1 := !(make_t1 (Split (res, next)));
         res
    in
    ref Memo.empty, init re (make_t1 Match)

  let full_match (memo, t) s =
    let rec follow ({ contents = (_, s) } as t) next =
      if S.mem t next then
        next
      else
        let next = S.add t next in
        match s with
        | Split (t1, t2) ->
           follow t1 next
           |> follow t2

        | _ ->
           next
    in
    let hit = ref 0 in
    let miss = ref 0 in
    let step curr c =
      try
        let res = Memo.find (curr, c) !memo in
        incr hit;
        res
      with Not_found ->
        incr miss;
        let res =
          S.fold
            (fun { contents = (_, s) } next ->
              match s with
              | CharSet (set, t) when CSet.mem c set ->
                 follow t next

              | _ ->
                 next
            ) curr S.empty
        in
        memo := Memo.add (curr, c) res !memo;
        res
    in
    let rec full_match l curr =
      match l with
      | [] ->
         S.mem (make_t1 Match) curr

      | c :: r ->
         step curr c
         |> full_match r
    in
    full_match (explode s) (follow t S.empty)

end
