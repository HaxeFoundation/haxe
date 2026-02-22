(*
	Code that should eventually be moved elsewhere.
*)

open Globals
open Ast
open Type

class texpr_builder (basic : basic_types) =
object(self)
	method assign (lhs : texpr) (rhs : texpr) =
		mk (TBinop(OpAssign,lhs,rhs)) lhs.etype (punion lhs.epos rhs.epos)

	method binop (op : binop) (lhs : texpr) (rhs : texpr) (t : Type.t) =
		mk (TBinop(op,lhs,rhs)) t (punion lhs.epos rhs.epos)

	method bool (b : bool) (p : pos) =
		mk (TConst (TBool b)) basic.tbool p

	method break (p : pos) =
		mk TBreak t_dynamic p

	method call (e1 : texpr) (el : texpr list) (tret : Type.t) =
		mk (TCall(e1,el)) tret (punion e1.epos (punion_el e1.epos el))

	method continue (p : pos) =
		mk TContinue t_dynamic p

	method local (v : tvar) (p : pos) =
		mk (TLocal v) v.v_type p

	method if_then (eif : texpr) (ethen : texpr) =
		mk (TIf(eif,ethen,None)) basic.tvoid (punion eif.epos ethen.epos)

	method if_then_else (eif : texpr) (ethen : texpr) (eelse : texpr) (t : Type.t) =
		mk (TIf(eif,ethen,Some eelse)) t (punion eif.epos eelse.epos)

	method instance_field (e : texpr) (c : tclass) (params : Type.t list) (cf : tclass_field) (t : Type.t) =
		mk (TField(e,FInstance(c,params,cf))) t e.epos

	method int (i : int) (p : pos) =
		mk (TConst (TInt (Int32.of_int i))) basic.tint p

	method meta0 (m : Meta.strict_meta) (e : texpr) =
		mk (TMeta((m,[],e.epos),e)) e.etype e.epos

	method meta1 (m : Meta.strict_meta) (marg1 : expr) (e : texpr) =
		mk (TMeta((m,[marg1],e.epos),e)) e.etype e.epos

	method null (t : Type.t) (p : pos) =
		mk (TConst TNull) t p

	method op_bool_and (e1 : texpr) (e2 : texpr) =
		self#binop OpBoolAnd e1 e2 basic.tbool

	method op_eq (e1 : texpr) (e2 : texpr) =
		self#binop OpEq e1 e2 basic.tbool

	method return (e : texpr) =
		mk (TReturn (Some e)) t_dynamic e.epos

	method static_field (e : texpr) (c : tclass) (cf : tclass_field) (t : Type.t) =
		mk (TField(e,FStatic(c,cf))) t e.epos

	method string (s : string) (p : pos) =
		mk (TConst (TString s)) basic.tstring p

	method super (t: Type.t) (p : pos) =
		mk (TConst TSuper) t p

	method this (t : Type.t) (p : pos) =
		mk (TConst TThis) t p

	method throw (e : texpr) =
		mk (TThrow e) t_dynamic e.epos

	method var_init (v : tvar) (e : texpr) =
		mk (TVar(v,Some e)) basic.tvoid (punion v.v_pos e.epos)

	method var_init_null (v : tvar) =
		self#var_init v (self#null v.v_type v.v_pos)

	method void_block (el : texpr list) =
		mk (TBlock el) basic.tvoid (Texpr.punion_el null_pos el)

end