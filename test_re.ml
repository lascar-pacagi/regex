let () = Printexc.record_backtrace true

(* Turn a regular expression from Regenerate into an RE one. *)
let rec to_re r =
  let open Regenerate.Regex in
  let open Regex in
  let concatenation_iter i re =
    1 -- (i - 1)
    |> List.fold_left (fun acc _ -> RE.concat re acc) re
  in
  match r with
  | One | Rep (0, Some 0, _) ->
     assert false

  | Set (true, l) ->
     CSet.of_list l
     |> RE.charset

  | Set (false, l) ->
     CSet.of_list l
     |> CSet.diff ascii_set
     |> RE.charset

  | Seq (One, re) | Seq (re, One) ->
     to_re re

  | Seq (re1, re2) ->
     RE.concat (to_re re1) (to_re re2)

  | Or (One, re) | Or (re, One) ->
     to_re re
     |> RE.zero_or_one

  | Or (re1, re2) ->
     RE.union (to_re re1) (to_re re2)

  | Rep (0, None, re) ->
     to_re re
     |> RE.zero_or_more

  | Rep (i, None, re) ->
     let re' = to_re re in
     RE.concat (concatenation_iter i re') (RE.zero_or_more re')

  | Rep (0, Some j, re) ->
     let re' = to_re re in
     concatenation_iter j (RE.zero_or_one re')

  | Rep (i, Some j, re) when i = j ->
     to_re re
     |> concatenation_iter i

  | Rep (i, Some j, re) ->
     let re' = to_re re in
     concatenation_iter (j - i) (RE.zero_or_one re')
     |> RE.concat (concatenation_iter i re')

  | And _ ->
     assert false

  | Not _ ->
     assert false

(* Check positive and negative samples. *)
let check (re, pos, neg) =
  let open Regex in
  let (matchers : (module Matching) list) =
    [ (module Backtracking); (module NFA); (module DFA)]
  in
  try
    let re = to_re re in
    List.for_all (fun (module M : Matching) ->
        let compiled_re = M.init re in
        List.for_all (fun s -> M.full_match compiled_re s) pos &&
          List.for_all (fun s -> not @@ M.full_match compiled_re s) neg)
    matchers
  with _ ->
      (* Discard regular expressions that Re does not handle. *)
      QCheck.assume_fail ()

let test =
  let alphabet = ['a'; 'b'; 'c'; 'd'] in
  let module Word = Regenerate.Word.String in
  let module Stream = Regenerate.Segments.ThunkList(Word) in
  let generator =
    Regenerate.arbitrary
      (module Word)   (* Datastructure for words *)
      (module Stream) (* Datastructure for streams of words *)
      ~compl:false    (* Should we generate complement operations? *)
      ~pp:Fmt.char    (* Printer for characters *)
      ~samples:50     (* Average number of samples for each regular expression *)
      ~skip:0
      alphabet
  in
  QCheck.Test.make ~count:1000 ~name:"qcheck" generator check

let make_test name regexp match_list nomatch_list =
  let open Regex in
  let re = RE.regex_from_string regexp in
  let backtracking = Backtracking.init re in
  let nfa = NFA.init re in
  let dfa = DFA.init re in
  let make_test' expected l =
    let msg =
      if expected then "result must be true with "
      else "result must be false with "
    in
    l
    |> List.map
         (fun s ->
           let f () =
             Alcotest.(check bool) "match backtracking" expected (Backtracking.full_match backtracking s)
           in
           let test1 = Alcotest.test_case (msg ^ "backtracking on: \"" ^ s ^ "\"") `Quick f in
           let f () =
             Alcotest.(check bool) "match nfa" expected (NFA.full_match nfa s)
           in
           let test2 = Alcotest.test_case (msg ^ "nfa on: \"" ^ s ^ "\"") `Quick f in
           let f () =
             Alcotest.(check bool) "match dfa" expected (DFA.full_match dfa s)
           in
           let test3 = Alcotest.test_case (msg ^ "dfa on: \"" ^ s ^ "\"") `Quick f in
           [test1; test2; test3]
         )
    |> List.flatten
  in
  (name, make_test' true match_list @ make_test' false nomatch_list)


let () =
    let test = QCheck_alcotest.to_alcotest test in
    Alcotest.run "regex"
      [
      make_test "regex: a*bc(de|fg)*" "a*bc(de|fg)*"
        ["aaabcdedede"; "bc"; "bcfg"; "abcfgdededefg"]
        ["aaaaaaaaab"; ""; "aaaabczfg"; "a"];

      make_test "regex: a\\**" "a\\**"
        ["a"; "a*"; "a**"; "a**********"]
        ["aaaaaaaaa"; ""; "aa"; "a*a*"];

      make_test "regex: [^a-z]+" "[^a-z]+"
        ["_++++AZ"; "A"; "*********"; "[^A-Z]+"; "[[[[[[[]]]]]]]"; "^^^^^\\"]
        ["aaaaaaaaa"; ""; "ABCDEFabcdef"; "[^a-z]+"; "z"];

      make_test "regex: [^a-z]+" "[^a-z]+"
        ["_++++AZ"; "A"; "*********"; "[^A-Z]+"; "[[[[[[[]]]]]]]"; "^^^^^\\"]
        ["aaaaaaaaa"; ""; "ABCDEFabcdef"; "[^a-z]+"; "z"];

      make_test "regex: ((0|1)(0|1)(0|1))*" "((0|1)(0|1)(0|1))*"
        [""; "010"; "111000"; "101010110"; "000000000000000000"; "111010111111000101110110111"]
        ["11"; "0"; "1110001"; "aaa"; "11100011100011"];

      make_test "regex: 0*(100*)*1?" "0*(100*)*1?"
        [""; "0"; "1"; "00"; "01"; "10"; "000"; "001"; "010"; "100"; "101"; "0000"; "0001"; "1010101010101010101010101"]
        ["11"; "01100"; "1000011"; "10110000011111"; "1111111111111"; "1a1a1a"];

      make_test "regex: (abc)+?" "(abc)+?"
        [""; "abc"; "abcabc"; "abcabcabc"]
        ["abca"; "abcab"; "bac"; "abcabcabcc"];

      make_test "multiple of 3" "(0|(1(01*0)*1))*"
        ["0"; "11"; "1111"; "1100000"]
        ["1"; "10"; "10001"; "100110001010101001"];

      make_test "multiple of 3 other version" "(0|11)*(10(1|00)*01)*"
        ["0"; "11"; "1111"; "1100000"]
        ["1"; "10"; "10001"; "100110001010101001"];

      make_test "wrong C comment" "/\\*.*\\*/"
        ["/**/"; "/**********************/"; "/*   :) :) :) :) ***** ///// */"; "/*/*/*/*/*abcdefgh*/";
         "/*.*------this is a comment*/"; "/* */ int toto() {} /* */"; "/* /* */ */"]
        [""; "/* abcdef"; "*/"; "/*  * /"];

      make_test "C comment" "/\\*(\\*+[^*/]+|[^*]+)*\\*+/"
        ["/**/"; "/**********************/"; "/*   :) :) :) :) ***** ///// */"; "/*.*------this is a comment*/"]
        [""; "/* abcdef"; "*/"; "/*  * /"; "/* */ int toto() {} /* */"; "/* /* */ */"; "/*/*/*/*/*abcdefgh*/"];

      make_test "identifier" "[_a-zA-Z][_a-zA-Z0-9]*"
        ["toto"; "__asm__"; "res42"; "MrCoder"; "_"; "_0123456789_"]
        [""; "12azerty"; "toto+"; "#MiniJava"; "1e+12"; "ab-cd"; " "];

      make_test "relational operators" "=(<|>|=)|(<|>)=?|!="
        ["=="; "!="; ">"; "<"; "<="; "=<"; ">="; "=>"]
        ["="; "=!"; "==="; "<<"; "> ="; "= <"; "> "];

      make_test "integer" "[-+]?(0x[0-9A-Fa-f]+|0b[01]+|0[0-7]*|[1-9][0-9]*)"
        ["1234567890"; "+1"; "-0b1"; "0xcafe"; "0777"; "1975"; "42"; "00000000000"]
        ["01789"; "0xabcdefg"; "198f"; "0b0111002"; "+-42"; "0000000009"];

      make_test "float" "[-+]?([0-9]+(\\.[0-9]*)?|\\.[0-9]+)([eE][-+]?[0-9]+)?"
        ["42"; ".0"; "0."; "1.e512"; "+23456e-2019"; "1E10"]
        ["."; "1,2"; "+-12.45"; "1980e"; "1980E+"];

      "random testing", [test]
    ]

(* let () =
 *   QCheck_runner.run_tests_main [test] *)
