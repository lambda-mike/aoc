// Library functions can be found here:
// https://ats-lang.github.io/DOCUMENT/ATS-Postiats/prelude/HTML/ATSPRE_all_in_one.html

#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
staload
UN = "prelude/SATS/unsafe.sats"

fun mytest(input: string): bool = case+ input of
  | "byr" => true
  | _ => false

fun mytest2{n:nat}(input: string(n)): bool = case+ list_of_list_vt(string_explode(input)) of
  // | cons('3', nil()) => true
  | cons(c1, cons(c2, nil())) when c1='3' && c2='4' => true
  | nil () => false
  | _ => false

// val xs = g0ofg1($list{int}(8, 2, 4, 3, 6, 5, 9, 0, 1, 7))
// val xs0 = list0_nil{int}() // xs = ()
//val xs0 = list0_nil{int}() // xs = ()
val () =
{
val x0 = 0
val x1 = 1
val zs: list(string, 0) = nil{string}()
val xs = nil{int}()
val xs = cons{int}(x0, cons{int}(x1, xs))
val+cons (x, xs) = xs
// val ble = 3 :: 2 :: 1 :: nil0()
val () = println! ("comp ", '0' <= '1' && '1' <= '9')
val () = println! ("isdigit ", isdigit('3'), isdigit('a'))
val () = println! ("isxdigit ", isxdigit('3'), isxdigit('a'), isxdigit('z'))
// val sss: string = "test"
// val sss2 = strptr2string(string_make_rlist(list_of_list_vt(string_explode(sss))))
// val sss2 = strptr2string(string_make_rlist_vt(string_explode("abc")))

// val sss3 = string_make_rlist_vt(string_explode("abc"))
val sss3 = string_make_rlist(list_of_list_vt(string_explode("abc")))
val () = println! ("sss3 ", sss3)
val () = free(sss3)
val f = "byr"
val rrr = mytest(f)
val rrr = mytest2(f)
val () = println! ("result ", rrr)
val rrr = mytest2("")
val () = println! ("result ", rrr)
val rrr = mytest2("z")
val () = println! ("result ", rrr)
val rrr = mytest2("3")
val () = println! ("result ", rrr)
val rrr = mytest2("34")
val () = println! ("result ", rrr)
val () = assertloc (x = x0)
val+cons (x, xs) = xs
val () = assertloc (x = x1)
val+nil() = xs
} (* end of [val] *)

// fun
// read_lines(fileRef: FILEref) =
//   g0ofg1(list0_nil{string}())
//   // g0ofg1($list{string}("1", "2", "3"))

implement main0 () = ()
