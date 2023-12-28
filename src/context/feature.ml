open Ast
open Type
open Error

let rec check_if_feature = function
	| [] -> []
	| (Meta.IfFeature,el,_) :: _ -> List.map (fun (e,p) -> match e with EConst (String(s,_)) -> s | _ -> raise_typing_error "String expected" p) el
	| _ :: l -> check_if_feature l

let set_feature m cf_ref s =
	m.m_extra.m_if_feature <- (s, cf_ref) :: m.m_extra.m_if_feature
