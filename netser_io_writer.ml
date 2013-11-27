type t = Buffer.t

let create () = Buffer.create 100

let contents b = Buffer.contents b

let write_char b c = Buffer.add_char b c

let write_u8 b x = Buffer.add_char b (Char.chr x)

let write_u16 b x =
    Buffer.add_char b (Char.chr (x lsr 8));
    Buffer.add_char b (Char.chr (x land 0xff))

let write_u16le b x =
    Buffer.add_char b (Char.chr (x land 0xff));
    Buffer.add_char b (Char.chr (x lsr 8))

let write_i32 b x =
    let high = Int32.to_int (Int32.shift_right_logical x 16) in
    write_u16 b high;
    let low = Int32.to_int x in
    write_u16 b low

let write_i32le b x =
    let low = Int32.to_int x in
    write_u16le b low;
    let high = Int32.to_int (Int32.shift_right_logical x 16) in
    write_u16le b high

let write_int b x =
    write_u16 b x;          (* low term *)
    let high = x lsr 16 in
    write_u16 b high

let write_float b x = write_i32 b (Int32.bits_of_float x)

let write_string b s = Buffer.add_string b s
