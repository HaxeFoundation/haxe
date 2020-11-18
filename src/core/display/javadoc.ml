type doc_tag =
	| Returns of string
	| Deprecated of string
	| Since of string
	| Default of string
	| See of string
	| Param of string * string
	| Throws of string * string
	| Event of string * string
	| Custom of string * string

let string_of_tag = function
	| Returns s -> "returns " ^ s
	| Deprecated s -> "deprecated " ^ s
	| Since s -> "since " ^ s
	| Default s -> "default " ^ s
	| See s -> "see " ^ s
	| Param(s1,s2) -> "param " ^ s1 ^ " " ^ s2
	| Throws(s1,s2) -> "throws " ^ s1 ^ " " ^ s2
	| Event(s1,s2) -> "event " ^ s1 ^ " " ^ s2
	| Custom(s1,s2) -> s1 ^ " " ^ s2

class javadoc (doc : string) = object(self)

	val mutable actual_doc = doc;
	val mutable tags = []
	val mutable parsed_tags = false

	method get_param_info (name : string) =
		if not parsed_tags then self#parse_tags;
		let rec loop tags = match tags with
			| (Param(name',value)) :: _ when name = name' ->
				Some value
			| _ :: tags ->
				loop tags
			| [] ->
				None
		in
		loop tags

	method parse_tags =
		parsed_tags <- true;
		let l = String.length doc in
		let commit_tag name value =
			let separate () =
				try
					ExtString.String.split value " "
				with _ ->
					value,""
			in
			let add tag = tags <- tag :: tags in
			match name with
			| "param" ->
				let name,doc = separate() in
				add (Param(name,doc))
			| "exception" | "throws" ->
				let name,doc = separate() in
				add (Throws(name,doc))
			| "event" ->
				let name,doc = separate() in
				add (Event(name,doc))
			| "deprecated" ->
				add (Deprecated value)
			| "return" | "returns" ->
				add (Returns value)
			| "since" ->
				add (Since value)
			| "default" ->
				add (Default value)
			| "see" ->
				add (See value)
			| _ ->
				add (Custom(name,value))
		in
		let read_until p c =
			let pn = String.index_from doc p c in
			String.sub doc p (pn - p),pn + 1
		in
		(* Returns the first non-whitespace character while counting indentation. *)
		let rec newline had_star indent p =
			if p >= l then
				None
			else begin
				let c = doc.[p] in
				match c with
				| ' ' | '\t' ->
					newline had_star (indent + 1) (p + 1)
				| '*' when not had_star ->
					newline true 0 (p + 1)
				| '\r' | '\n' ->
					newline false 0 (p + 1)
				| _ ->
					Some(c,p + 1,indent)
			end
		in
		let newline p = newline false 0 p in
		(* Returns (position of last real char, position of next char) *)
		let last_char_pos_on_line p =
			let rec loop pl pn =
				if pn = l then
					pl,pn
				else match doc.[pn] with
					| ' ' | '\r' | '\t' ->
						loop pl (pn + 1)
					| '\n' ->
						pl,pn + 1
					| _ ->
						loop pn (pn + 1)
			in
			loop p p
		in
		let doc_buf = Buffer.create 0 in
		let rec loop p = match newline p with
			| None ->
				()
			| Some(c,p,indent) ->
				match c with
				| '@' ->
					begin try
						let name,pn = read_until p ' ' in
						let value_buf = Buffer.create 0 in
						(* Append lines that have higher indentation. *)
						let rec loop_value ps =
							let pl,pn = last_char_pos_on_line ps in
							Buffer.add_substring value_buf doc ps (pl - ps + 1);
							match newline pn with
							| Some(c,p,indent') when indent' > indent ->
								Buffer.add_char value_buf '\n';
								Buffer.add_char value_buf c;
								loop_value p
							| _ ->
								pn
						in
						let pn = loop_value pn in
						commit_tag name (Buffer.contents value_buf);
						loop pn
					with Not_found ->
						Buffer.add_substring doc_buf doc p (l - p)
					end
				| _ ->
					Buffer.add_char doc_buf c;
					let pl,pn = last_char_pos_on_line p in
					Buffer.add_substring doc_buf doc p (pl - p + 1);
					loop pn
		in
		loop 0;
		tags <- List.rev tags;
end