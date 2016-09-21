open Ast

module SymbolKind = struct
	type t =
		| Class
		| Interface
		| Enum
		| Typedef
		| Abstract
		| Field
		| Property
		| Method
		| Constructor
		| Function
		| Variable

	let to_int = function
		| Class -> 1
		| Interface -> 2
		| Enum -> 3
		| Typedef -> 4
		| Abstract -> 5
		| Field -> 6
		| Property -> 7
		| Method -> 8
		| Constructor -> 9
		| Function -> 10
		| Variable -> 11
end

module SymbolInformation = struct
	type t = {
		name : string;
		kind : SymbolKind.t;
		pos : Globals.pos;
		container_name : string option;
	}

	let make name kind pos container_name = {
		name = name;
		kind = kind;
		pos = pos;
		container_name = container_name;
	}
end

module DiagnosticsSeverity = struct
	type t =
		| Error
		| Warning
		| Information
		| Hint

	let to_int = function
		| Error -> 1
		| Warning -> 2
		| Information -> 3
		| Hint -> 4
end