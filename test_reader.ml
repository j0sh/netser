open Netser_io_reader
open OUnit

let rs () =
    let r = create () in
    append r "\000\123\213\090";
    r

let test_char () =
    let r = rs () in
    assert_equal 0 (Char.code (read_char r));
    assert_equal 123 (Char.code (read_char r));
    assert_equal 213 (Char.code (read_char r));
    assert_equal 90 (Char.code (read_char r))

let test_u8 () =
    let r = rs () in
    assert_equal 0   (read_u8 r);
    assert_equal 123 (read_u8 r);
    assert_equal 213 (read_u8 r);
    assert_equal 90  (read_u8 r)

let test_u16 () =
    let r = rs () in
    assert_equal 123 (read_u16 r);
    assert_equal 54618 (read_u16 r)

let test_int () =
    let r = rs () in
    assert_equal 8115546 (read_int r)

let test_int32 () =
    let r = rs () in
    assert_equal 8115546l (read_int32 r)

let test_append () =
    let r = create () in
    append r "a";
    assert_equal 'a' (read_char r);
    append r "bcdef";
    assert_equal 'b' (read_char r);
    assert_equal 'c' (read_char r);
    assert_equal 'd' (read_char r);
    assert_equal 'e' (read_char r);
    assert_equal 'f' (read_char r)

let test_string () =
    let r = create () in
    append r "asdfqwerty";
    assert_equal "asdf" (read_string r 4);
    assert_equal "qwerty" (read_string r 6)

let test_float () =
    let r = create () in
    let pi_s = "\x40\x49\x0f\xd0" in
    let e_s  = "\x40\x2d\xf8\x4d" in
    append r pi_s;
    append r e_s;
    let pi_f = 3.14159 in
    let e_f  = 2.71828 in
    let printer = Printf.sprintf "%f\n" in
    let fixed f = int_of_float (f *. 100000.) in
    let cmp exp real = ((fixed exp) = (fixed real)) in
    assert_equal ~cmp ~printer pi_f  (read_float r);
    assert_equal ~cmp ~printer e_f   (read_float r)

let suite = "suite" >::: [
    "test_char">::test_char;
    "test_u8">::test_u8;
    "test_u16">::test_u16;
    "test_int">::test_int;
    "test_int32">::test_int32;
    "test_append">::test_append;
    "test_string">::test_string;
    "test_float">::test_float;
]

let _ = run_test_tt_main suite
