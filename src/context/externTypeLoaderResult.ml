open Globals
open Ast

type t =
	| NoType
	| TypeDefinition of Ast.package
	| PathForwarding of path
