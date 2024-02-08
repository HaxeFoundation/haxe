// Taken from vshaxe... not ideal to copy it here

private enum abstract ModuleSymbolKind(Int) {
	var Class = 1;
	var Interface;
	var Enum;
	var TypeAlias;
	var Abstract;
	var Field;
	var Property;
	var Method;
	var Constructor;
	var Function;
	var Variable;
	var Struct;
	var EnumAbstract;
	var Operator;
	var EnumMember;
	var Constant;
}

typedef ModuleSymbolEntry = {
	var name:String;
	var kind:ModuleSymbolKind;
	// var range:Range;
	var ?containerName:String;
	var ?isDeprecated:Bool;
}
