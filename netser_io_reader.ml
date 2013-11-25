type t = {
    buf: Buffer.t;
    mutable pos: int;
    mutable str: string;
}

open EndianString
module BE = BigEndian

let create () = { buf = Buffer.create 100; pos = 0; str = "" }

let pos b = b.pos

let set_pos b pos = b.pos <- pos

let append b s = Buffer.add_string b.buf s;
    b.str <- Buffer.contents b.buf

let read_char b =
    let c = b.str.[b.pos] in
    b.pos <- b.pos + 1;
    c

let read_u8 b =
    Char.code (read_char b)

let read_u16 b =
    let i = BE.get_uint16 b.str b.pos in
    b.pos <- b.pos + 2;
    i

let read_int32 b =
    let i = BE.get_int32 b.str b.pos in
    b.pos <- b.pos + 4;
    i

let read_int b = Int32.to_int (read_int32 b)

let read_float b = Int32.float_of_bits (read_int32 b)

let read_string b l =
    let s = String.sub b.str b.pos l in
    b.pos <- b.pos + l;
    s
