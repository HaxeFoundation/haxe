package refactor.typing;

import refactor.discover.Type;

enum TypeHintType {
	ClasspathType(type:Type, typeParams:Array<TypeHintType>);
	LibType(name:String, fullName:String, typeParams:Array<TypeHintType>);
	FunctionType(args:Array<TypeHintType>, retVal:Null<TypeHintType>);
	StructType(fields:Array<TypeHintType>);
	NamedType(name:String, typeHint:Null<TypeHintType>);
	UnknownType(name:String);
}
