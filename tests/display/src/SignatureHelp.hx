// from vshaxe

typedef SignatureHelp = {
	var signatures:Array<SignatureInformation>;
	var ?activeSignature:Int;
	var ?activeParameter:Int;
}

typedef SignatureInformation = {
	var label:String;
	var ?documentation:String;
	var ?parameters:Array<ParameterInformation>;
}

typedef ParameterInformation = {
	var label:String;
	var ?documentation:String;
}
