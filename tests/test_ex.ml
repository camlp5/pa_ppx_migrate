open OUnit2

open Ex_ast
open Ex_migrate

let loc0 = Ploc.make_unlined (1,2)

let test_ast1_ast2 ctxt =
  let dt = Migrate_AST1_AST2.make_dt () in
  assert_equal AST2.({ it = C 1 ; z = `A ; z2 = `C ; extra = 3; new_field = 3 }) 
    Migrate_AST1_AST2.(migrate_t4 dt AST1.{ it = C true ; z = `A ; z2 = `C ; extra = 3 ; dropped_field = "1" })

let test_ast2_ast1 ctxt =
  let dt = Migrate_AST2_AST1.make_dt () in
  assert_equal AST1.(A(loc0, "1", [2;3],(43,false))) 
    Migrate_AST2_AST1.(dt.migrate_t1 dt AST2.(A(loc0, 1, [2;3],(43,43,false))))

let test_ast5 ctxt =
  let dt = Migrate_AST5.make_dt () in
  assert_equal AST5.(A 1) 
    Migrate_AST5.(dt.migrate_t dt AST5.(A 1))

let test_ast6 ctxt =
  let dt = Migrate_AST6.make_dt () in
  assert_equal AST6.(`C 1) 
    Migrate_AST6.(dt.migrate_t dt AST6.(`C 1))

let test_ast7 ctxt =
  let dt = Migrate_AST7.make_dt () in
  assert_equal AST7.(`C 1) 
    Migrate_AST7.(dt.migrate_t' dt AST6.(`C 1))
; assert_equal AST7.(`D 1) 
    Migrate_AST7.(dt.migrate_t' dt AST6.(`D 1))

let suite = "test_ex" >::: [
    "test_ast1_ast2"   >:: test_ast1_ast2
  ; "test_ast2_ast1"   >:: test_ast2_ast1
  ; "test_ast5"   >:: test_ast5
  ; "test_ast6"   >:: test_ast6
  ; "test_ast7"   >:: test_ast7
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
