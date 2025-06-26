open Type
open MatcherGlobals

class subject (e : texpr) (bind : Bind.bind option) = object (self)
	method get_expr =
		begin match bind with
		| None ->
			()
		| Some bind ->
			bind.b_status <- BindUsed;
		end;
		e

	method get_scoped_expr = match bind with
		| Some ({b_status = BindDeferred} as bind) ->
			(* For deferred bindings, we generate `subject = expr` and mark the binding as used so that
			   subsequent calls to this function do not generate the assignment again. The returned function
			   can be used to reset the status back to BindDeferred, for cases where we need to initalize
			   again in an unrelated scope. *)
			let e = Texpr.Builder.binop OpAssign e bind.b_expr e.etype e.epos in
			bind.b_status <- BindUsed;
			e,(fun () ->
				bind.b_status <- BindDeferred
			)
		| Some ({b_status = BindUnused} as bind) ->
			bind.b_status <- BindUsed;
			e,(fun () -> ())
		| _ ->
			e,(fun () -> ())

	method get_assigned_expr =
		let e,reset = self#get_scoped_expr in
		reset();
		e

	method get_pos =
		e.epos

	method to_string =
		s_expr_pretty e
end