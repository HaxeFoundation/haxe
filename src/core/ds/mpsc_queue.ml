(* Borrowed from saturn until we can figure out the dependency mess *)

(* A lock-free multi-producer, single-consumer, thread-safe queue without support for cancellation.
   This makes a good data structure for a scheduler's run queue.

   See: "Implementing lock-free queues"
   https://people.cs.pitt.edu/~jacklange/teaching/cs2510-f12/papers/implementing_lock_free.pdf

   It is simplified slightly because we don't need multiple consumers.
   Therefore [head] is not atomic. *)

   exception Closed

   module Node : sig
	 type 'a t = { next : 'a opt Atomic.t; mutable value : 'a }
	 and +'a opt

	 val make : next:'a opt -> 'a -> 'a t

	 val none : 'a opt
	 (** [t.next = none] means that [t] is currently the last node. *)

	 val closed : 'a opt
	 (** [t.next = closed] means that [t] will always be the last node. *)

	 val some : 'a t -> 'a opt
	 val fold : 'a opt -> none:(unit -> 'b) -> some:('a t -> 'b) -> 'b
   end = struct
	 (* https://github.com/ocaml/RFCs/pull/14 should remove the need for magic here *)

	 type +'a opt (* special | 'a t *)
	 type 'a t = { next : 'a opt Atomic.t; mutable value : 'a }
	 type special = Nothing | Closed

	 let none : 'a. 'a opt = Obj.magic Nothing
	 let closed : 'a. 'a opt = Obj.magic Closed
	 let some (t : 'a t) : 'a opt = Obj.magic t

	 let fold (opt : 'a opt) ~none:n ~some =
	   if opt == none then n ()
	   else if opt == closed then raise Closed
	   else some (Obj.magic opt : 'a t)

	 let make ~next value = { value; next = Atomic.make next }
   end

   type 'a t = { tail : 'a Node.t Atomic.t; mutable head : 'a Node.t }
   (* [head] is the last node dequeued (or a dummy node, initially).
	  [head.next] gives the real first node, if not [Node.none].
	  If [tail.next] is [none] then it is the last node in the queue.
	  Otherwise, [tail.next] is a node that is closer to the tail. *)

   let push t x =
	 let node = Node.(make ~next:none) x in
	 let rec aux () =
	   let p = Atomic.get t.tail in
	   (* While [p.next == none], [p] is the last node in the queue. *)
	   if Atomic.compare_and_set p.next Node.none (Node.some node) then
		 (* [node] has now been added to the queue (and possibly even consumed).
			Update [tail], unless someone else already did it for us. *)
		 ignore (Atomic.compare_and_set t.tail p node : bool)
	   else
		 (* Someone else added a different node first ([p.next] is not [none]).
			Make [t.tail] more up-to-date, if it hasn't already changed, and try again. *)
		 Node.fold (Atomic.get p.next)
		   ~none:(fun () -> assert false)
		   ~some:(fun p_next ->
			 ignore (Atomic.compare_and_set t.tail p p_next : bool);
			 aux ())
	 in
	 aux ()

   let rec push_head t x =
	 let p = t.head in
	 let next = Atomic.get p.next in
	 if next == Node.closed then raise Closed;
	 let node = Node.make ~next x in
	 if Atomic.compare_and_set p.next next (Node.some node) then
	   if
		 (* We don't want to let [tail] get too far behind, so if the queue was empty, move it to the new node. *)
		 next == Node.none
	   then ignore (Atomic.compare_and_set t.tail p node : bool)
	   else
		 ( (* If the queue wasn't empty, there's nothing to do.
			  Either tail isn't at head or there is some [push] thread working to update it.
			  Either [push] will update it directly to the new tail, or will update it to [node]
			  and then retry. Either way, it ends up at the real tail. *) )
	 else (
	   (* Someone else changed it first. This can only happen if the queue was empty. *)
	   assert (next == Node.none);
	   push_head t x)

   let rec close (t : 'a t) =
	 (* Mark the tail node as final. *)
	 let p = Atomic.get t.tail in
	 if not (Atomic.compare_and_set p.next Node.none Node.closed) then
	   (* CAS failed because [p] is no longer the tail (or is already closed). *)
	   Node.fold (Atomic.get p.next)
		 ~none:(fun () -> assert false)
		   (* Can't switch from another state to [none] *)
		 ~some:(fun p_next ->
		   (* Make [tail] more up-to-date if it hasn't changed already *)
		   ignore (Atomic.compare_and_set t.tail p p_next : bool);
		   (* Retry *)
		   close t)

   let pop t =
	 let p = t.head in
	 (* [p] is the previously-popped item. *)
	 let node = Atomic.get p.next in
	 Node.fold node
	   ~none:(fun () -> None)
	   ~some:(fun node ->
		 t.head <- node;
		 let v = node.value in
		 node.value <- Obj.magic ();
		 (* So it can be GC'd *)
		 Some v)

   let is_empty t =
	 Node.fold (Atomic.get t.head.next)
	   ~none:(fun () -> true)
	   ~some:(fun _ -> false)

   let create () =
	 let dummy = { Node.value = Obj.magic (); next = Atomic.make Node.none } in
	 { tail = Atomic.make dummy; head = dummy }
