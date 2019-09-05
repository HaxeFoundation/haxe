type priority = int

type 'a t =
| Empty
| Node of priority * 'a * 'a t * 'a t

let empty = Empty

let rec insert queue prio elt = match queue with
	| Empty -> Node(prio, elt, Empty, Empty)
	| Node(p, e, left, right) ->
		if prio <= p then
			Node(prio, elt, insert right p e, left)
		else
			Node(p, e, insert right prio elt, left)

exception Queue_is_empty

let rec remove_top = function
	| Empty -> raise Queue_is_empty
	| Node(prio, elt, left, Empty) -> left
	| Node(prio, elt, Empty, right) -> right
	| Node(prio, elt, (Node(lprio, lelt, _, _) as left), (Node(rprio, relt, _, _) as right)) ->
		if lprio <= rprio then
			Node(lprio, lelt, remove_top left, right)
		else
			Node(rprio, relt, left, remove_top right)

let extract = function
	| Empty -> raise Queue_is_empty
	| Node(prio, elt, _, _) as queue -> (prio, elt, remove_top queue)

let is_empty = function
	| Empty -> true
	| Node _ -> false

let fold queue f acc =
	let rec loop queue acc = match queue with
		| Empty -> acc
		| Node(prio, elt, left, Empty) -> loop left (f acc prio elt)
		| Node(prio, elt, Empty, right) -> loop right (f acc prio elt)
		| Node(prio, elt, (Node(lprio,_,_,_) as left), (Node(rprio,relt,_,_) as right)) ->
			let acc = f acc prio elt in
			if lprio <= rprio then begin
				let acc = loop left acc in
				loop right acc
			end else begin
				let acc = loop right acc in
				loop left acc
			end
	in
	loop queue acc

let merge queue1 queue2 =
	fold queue1 insert queue2