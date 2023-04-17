open Globals
open Type

type t_kind =
	| ConConst of tconstant
	| ConEnum of tenum * tenum_field
	| ConStatic of tclass * tclass_field
	| ConTypeExpr of module_type
	| ConFields of string list
	| ConArray of int

type t = t_kind * pos

let to_string con = match fst con with
	| ConConst ct -> s_const ct
	| ConEnum(en,ef) -> ef.ef_name
	| ConStatic(c,cf) -> Printf.sprintf "%s.%s" (s_type_path (match c.cl_kind with KAbstractImpl a -> a.a_path | _ -> c.cl_path)) cf.cf_name
	| ConTypeExpr mt -> s_type_path (t_infos mt).mt_path
	| ConFields fields -> Printf.sprintf "{ %s }" (String.concat ", " fields)
	| ConArray i -> Printf.sprintf "<array %i>" i

let equal con1 con2 = match fst con1,fst con2 with
	| ConConst ct1,ConConst ct2 -> ct1 = ct2
	| ConEnum(en1,ef1),ConEnum(en2,ef2) -> en1 == en2 && ef1 == ef2
	| ConStatic(c1,cf1),ConStatic(c2,cf2) -> c1 == c2 && cf1 == cf2
	| ConTypeExpr mt1,ConTypeExpr mt2 -> mt1 == mt2
	| ConFields _,ConFields _ -> true
	| ConArray i1,ConArray i2 -> i1 = i2
	| _ -> false

let arity con = match fst con with
	| ConEnum (_,{ef_type = TFun(args,_)}) -> List.length args
	| ConEnum _ -> 0
	| ConConst _ -> 0
	| ConFields fields -> List.length fields
	| ConArray i -> i
	| ConTypeExpr _ -> 0
	| ConStatic _ -> 0

let compare con1 con2 = match fst con1,fst con2 with
	| ConConst ct1,ConConst ct2 -> compare ct1 ct2
	| ConEnum(en1,ef1),ConEnum(en2,ef2) -> compare ef1.ef_index ef2.ef_index
	| ConStatic(c1,cf1),ConStatic(c2,cf2) -> compare cf1.cf_name cf2.cf_name
	| ConTypeExpr mt1,ConTypeExpr mt2 -> compare (t_infos mt1).mt_path (t_infos mt2).mt_path
	| ConFields _,ConFields _ -> 0
	| ConArray i1,ConArray i2 -> i1 - i2
	| _ -> -1 (* Could assert... *)

let hash con = Hashtbl.hash (fst con)