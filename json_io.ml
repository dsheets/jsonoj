type t = Json_type.t
open Json_type

(*** Parsing ***)

let check_string_is_utf8 s =
  let encoding =
    if String.length s < 4 then `UTF8
    else Json_lexer.detect_encoding s.[0] s.[1] s.[2] s.[3] in
  if encoding <> `UTF8 then
    json_error "Only UTF-8 encoding is supported" 

let json_of_string 
    ?allow_comments
    ?allow_nan
    ?big_int_mode
    s =
  check_string_is_utf8 s;
  let p = Json_lexer.make_param ?allow_comments ?allow_nan ?big_int_mode () in
  let j = 
    Json_parser.main 
      (Json_lexer.token p)
      (Lexing.from_string s)
  in
  j


(*** Printing ***)

(* JSON does not allow rendering floats with a trailing dot: that is,
   1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
   string_of_int result in a trailing dot *)
let fprint_float allow_nan fmt f =
  match classify_float f with
      FP_nan -> 
	if allow_nan then Format.fprintf fmt "NaN"
	else json_error "Not allowed to serialize NaN value"
    | FP_infinite ->
	if allow_nan then
	  if f < 0. then Format.fprintf fmt "-Infinity"
	  else Format.fprintf fmt "Infinity"
	else json_error "Not allowed to serialize infinite value"
    | FP_zero
    | FP_normal
    | FP_subnormal ->
	let s = string_of_float f in
	Format.fprintf fmt "%s" s;
	let s_len = String.length s in
	if s.[ s_len - 1 ] = '.' then
	  Format.fprintf fmt "0"

let hex n =
  Char.chr (
    if n < 10 then 
      n + 48
    else 
      n + 87
  )

let escape_json_string add_string add_char s =
  for i = 0 to String.length s - 1 do
    let c = String.unsafe_get s i in
    match c with 
      | '"'    -> add_string "\\\""
      | '\t'   -> add_string "\\t"
      | '\r'   -> add_string "\\r"
      | '\b'   -> add_string "\\b"
      | '\n'   -> add_string "\\n"
      | '\012' -> add_string "\\f"
      | '\\'   -> add_string "\\\\"
   (* | '/'    -> "\\/" *) (* Forward slash can be escaped 
			      but doesn't have to *)
      | '\x00'..'\x1F' (* Control characters that must be escaped *)
      | '\x7F' (* DEL *) -> 

        add_string "\\u00";
        let code = Char.code c in
        add_char (hex (code lsr 4));
        add_char (hex (code land 0xf));

      | _  -> 
	(* Don't bother detecting or escaping multibyte chars *)
	add_char c
  done

(* Determine whether a string as any characters that would need to
   be escaped *)
let rec has_char_to_escape s len i =
  if i < len then (
    let c = String.unsafe_get s i in
    match c with
      | '"' | '\t' | '\r' | '\b' | '\n' | '\012' | '\\' (*| '/'*)
      | '\x00'..'\x1F' | '\x7F' -> true
      | _ -> has_char_to_escape s len (i+1)
  )
  else
    false

let has_char_to_escape s =
  has_char_to_escape s (String.length s) 0

let fquote_json_string fmt s =
  let buf = Buffer.create (String.length s) in
  escape_json_string (Buffer.add_string buf) (Buffer.add_char buf) s;
  Format.fprintf fmt "\"%s\"" (Buffer.contents buf)

let bquote_json_string buf s =
  Buffer.add_string buf "\"";

  (* avoid adding strings, which commonly contain no characters needing escape, 
     one character at-a-time.  *)
  if has_char_to_escape s then
    escape_json_string (Buffer.add_string buf) (Buffer.add_char buf) s
  else
    Buffer.add_string buf s;

  Buffer.add_string buf "\""

let rec bprint_json allow_nan buf = function
  | Object o -> 
    Buffer.add_string buf "{";
    bprint_object allow_nan buf o;
    Buffer.add_string buf "}"

  | Array a -> 
    Buffer.add_string buf "[";
    bprint_list allow_nan buf a;
    Buffer.add_string buf "]"

  | Bool b -> 
    Buffer.add_string buf (if b then "true" else "false")

  | Null -> 
    Buffer.add_string buf "null"

  | Int i -> 
    Buffer.add_string buf (string_of_int i)

  | Float f -> 
    Buffer.add_string buf (string_of_json_float ~allow_nan f)

  | String s -> 
    bquote_json_string buf s
      
and bprint_list allow_nan buf = function
  | [] -> ()
  | [x] -> bprint_json allow_nan buf x
  | x :: tl -> 
    bprint_json allow_nan buf x;
    Buffer.add_string buf ","; 
    bprint_list allow_nan buf tl
      
and bprint_object allow_nan buf = function
  | [] -> ()
  | [x] -> bprint_pair allow_nan buf x
  | x :: tl -> 
    bprint_pair allow_nan buf x;
    Buffer.add_string buf ","; 
    bprint_object allow_nan buf tl

and bprint_pair allow_nan buf (key, x) =
  bquote_json_string buf key;
  Buffer.add_string buf ":";
  bprint_json allow_nan buf x

(* json does not allow rendering floats with a trailing dot: that is,
   1234. is not allowed, but 1234.0 is ok.  here, we add a '0' if
   string_of_int result in a trailing dot *)
and string_of_json_float ?(allow_nan=false) f =
  match classify_float f with
    | FP_nan -> 
      if allow_nan then "NaN"
      else json_error "Not allowed to serialize NaN value"
    | FP_infinite ->
      if allow_nan then
	if f < 0. then "-Infinity"
	else "Infinity"
      else json_error "Not allowed to serialize infinite value"
    | FP_zero
    | FP_normal
    | FP_subnormal ->
      let s = string_of_float f in
      let s_len = String.length s in
      if s.[ s_len - 1 ] = '.' then
	s ^ "0"
      else
	s

module Pretty =
struct
  open Format
    
  (* Printing anything but a value in a key:value pair.

     Opening and closing brackets in such arrays and objects
     are aligned vertically if they are not on the same line. 
  *)
  let rec fprint_json allow_nan fmt = function
      Object l -> fprint_object allow_nan fmt l
    | Array l -> fprint_array allow_nan fmt l
    | Bool b -> fprintf fmt "%s" (if b then "true" else "false")
    | Null -> fprintf fmt "null"
    | Int i -> fprintf fmt "%i" i
    | Float f -> fprint_float allow_nan fmt f
    | String s -> fquote_json_string fmt s
	
  (* Printing an array which is not the value in a key:value pair *)
  and fprint_array allow_nan fmt = function
      [] -> fprintf fmt "[]"
    | x :: tl -> 
	fprintf fmt "@[<hv 2>[@ "; 
	fprint_json allow_nan fmt x;
	List.iter (fun x -> 
		     fprintf fmt ",@ ";
		     fprint_json allow_nan fmt x) tl;
	fprintf fmt "@;<1 -2>]@]"
	  
  (* Printing an object which is not the value in a key:value pair *)
  and fprint_object allow_nan fmt = function
      [] -> fprintf fmt "{}"
    | x :: tl -> 
	fprintf fmt "@[<hv 2>{@ "; 
	fprint_pair allow_nan fmt x;
	List.iter (fun x -> 
		     fprintf fmt ",@ ";
		     fprint_pair allow_nan fmt x) tl;
	fprintf fmt "@;<1 -2>}@]"

  (* Printing a key:value pair.

     The opening bracket stays on the same line as the key, no matter what,
     and the closing bracket is either on the same line
     or vertically aligned with the beginning of the key. 
  *)
  and fprint_pair allow_nan fmt (key, x) =
    match x with
	Object l -> 
	  (match l with
	       [] -> fprintf fmt "%a: {}" fquote_json_string key
	     | x :: tl -> 
		 fprintf fmt "@[<hv 2>%a: {@ " fquote_json_string key;
		 fprint_pair allow_nan fmt x;
		 List.iter (fun x -> 
			      fprintf fmt ",@ ";
			      fprint_pair allow_nan fmt x) tl;
		 fprintf fmt "@;<1 -2>}@]")
      | Array l ->
	  (match l with
	       [] -> fprintf fmt "%a: []" fquote_json_string key
	     | x :: tl -> 
		 fprintf fmt "@[<hv 2>%a: [@ " fquote_json_string key;
		 fprint_json allow_nan fmt x;
		 List.iter (fun x -> 
			      fprintf fmt ",@ ";
			      fprint_json allow_nan fmt x) tl;
		 fprintf fmt "@;<1 -2>]@]")
      | _ -> 
	  (* An atom, perhaps a long string that would go to the next line *)
	  fprintf fmt "@[%a:@;<1 2>%a@]" 
	    fquote_json_string key (fprint_json allow_nan) x

  let print ?(allow_nan = false) ?(recursive = false) fmt x =
    if not recursive then
      Browse.assert_object_or_array x;
    fprint_json allow_nan fmt x
end

let string_of_json ?(allow_nan=false) ?(compact=false) x =
  let buf = Buffer.create 2000 in
  if compact then bprint_json allow_nan buf x
  else (let fmt = Format.formatter_of_buffer buf in
        Browse.assert_object_or_array x;
        Pretty.fprint_json allow_nan fmt x;
        Format.pp_print_flush fmt ());
  Buffer.contents buf

