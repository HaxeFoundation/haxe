package hl.types;

@:keep
class TypeDecl {
	public var type : Type;
}

@:keep
class Class extends TypeDecl {
	public var __name__ : String;
}