open Stdlib

type t = {
  text : string;
  encoding : [ `UTF8 | `UTF16 ];
  line_starts : int array;
}

let make ?(encoding = `UTF8) text =
  let next_line from = Option.map (fun i -> i + 1, i + 1) (String.index_from_opt text from '\n') in
  { text; encoding; line_starts = Array.of_seq (Seq.cons 0 (Seq.unfold next_line 0)) }

let of_file ?encoding path = make ?encoding (In_channel.with_open_bin path In_channel.input_all)

let step t i =
  let decode = String.get_utf_8_uchar t.text i in
  let bytes = Uchar.utf_decode_length decode in
  match t.encoding with
  | `UTF8 -> bytes, bytes
  | `UTF16 -> bytes, Uchar.utf_16_byte_length (Uchar.utf_decode_uchar decode) / 2

let valid_offset t offset = Int.max 0 (Int.min (String.length t.text) offset)

let line t offset =
  let offset = valid_offset t offset in
  let starts = t.line_starts in
  let rec search lo hi =
    if hi - lo = 1 then lo
    else
      let mid = (lo + hi) / 2 in
      if starts.(mid) <= offset then search mid hi else search lo mid
  in
  search 0 (Array.length starts)

let position t offset =
  let offset = valid_offset t offset in
  let line = line t offset in
  let rec loop i units =
    if i >= offset then units
    else let (bytes, width) = step t i in loop (i + bytes) (units + width)
  in
  line, loop t.line_starts.(line) 0

let offset t ~line ~character =
  let starts = t.line_starts in
  let len = Array.length starts in
  if line < 0 then 0
  else if line >= len then String.length t.text
  else
    let eol = if line + 1 < len then starts.(line + 1) - 1 else String.length t.text in
    let rec loop i units =
      if units >= character || i >= eol then i
      else let (bytes, width) = step t i in loop (i + bytes) (units + width)
    in
    loop starts.(line) 0
