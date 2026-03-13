package haxeparser;

import haxe.macro.Expr;

enum Keyword {
	KwdFunction;
	KwdClass;
	KwdVar;
	KwdIf;
	KwdElse;
	KwdWhile;
	KwdDo;
	KwdFor;
	KwdBreak;
	KwdContinue;
	KwdReturn;
	KwdExtends;
	KwdImplements;
	KwdImport;
	KwdSwitch;
	KwdCase;
	KwdDefault;
	KwdStatic;
	KwdPublic;
	KwdPrivate;
	KwdTry;
	KwdCatch;
	KwdNew;
	KwdThis;
	KwdThrow;
	KwdExtern;
	KwdEnum;
	KwdIn;
	KwdInterface;
	KwdUntyped;
	KwdCast;
	KwdOverride;
	KwdTypedef;
	KwdDynamic;
	KwdPackage;
	KwdInline;
	KwdUsing;
	KwdNull;
	KwdTrue;
	KwdFalse;
	KwdAbstract;
	KwdMacro;
}

class KeywordPrinter {
	static public function toString(kwd:Keyword) {
		return switch(kwd) {
			case KwdFunction: "function";
			case KwdClass: "class";
			case KwdVar: "var";
			case KwdIf: "if";
			case KwdElse: "else";
			case KwdWhile: "while";
			case KwdDo: "do";
			case KwdFor: "for";
			case KwdBreak: "break";
			case KwdContinue: "continue";
			case KwdReturn: "return";
			case KwdExtends: "extends";
			case KwdImplements: "implements";
			case KwdImport: "import";
			case KwdSwitch: "switch";
			case KwdCase: "case";
			case KwdDefault: "default";
			case KwdStatic: "static";
			case KwdPublic: "public";
			case KwdPrivate: "private";
			case KwdTry: "try";
			case KwdCatch: "catch";
			case KwdNew: "new";
			case KwdThis: "this";
			case KwdThrow: "throw";
			case KwdExtern: "extern";
			case KwdEnum: "enum";
			case KwdIn: "in";
			case KwdInterface: "interface";
			case KwdUntyped: "untyped";
			case KwdCast: "cast";
			case KwdOverride: "override";
			case KwdTypedef: "typedef";
 			case KwdDynamic: "dynamic";
			case KwdPackage: "package";
			case KwdInline: "inline";
			case KwdUsing: "using";
			case KwdNull: "null";
			case KwdTrue: "true";
			case KwdFalse: "false";
			case KwdAbstract: "abstract";
			case KwdMacro: "macro";
		}
	}
}

enum TokenDef {
	Kwd(k:Keyword);
	Const(c:haxe.macro.Expr.Constant);
	Sharp(s:String);
	Dollar(s:String);
	Unop(op:haxe.macro.Expr.Unop);
	Binop(op:haxe.macro.Expr.Binop);
	Comment(s:String);
	CommentLine(s:String);
	IntInterval(s:String);
	Semicolon;
	Dot;
	DblDot;
	Arrow;
	Comma;
	BkOpen;
	BkClose;
	BrOpen;
	BrClose;
	POpen;
	PClose;
	Question;
	At;
	Eof;
}

class TokenDefPrinter {
	static public function toString(def:TokenDef) {
		return switch(def) {
			case Kwd(k): k.getName().substr(3).toLowerCase();
			case Const(CInt(s) | CFloat(s) | CIdent(s)): s;
			case Const(CString(s)): '"$s"';
			case Const(CRegexp(r, opt)): '~/$r/$opt';
			case Sharp(s): '#$s';
			case Dollar(s): '$$$s';
			case Unop(op): new haxe.macro.Printer("").printUnop(op);
			case Binop(op): new haxe.macro.Printer("").printBinop(op);
			case Comment(s): '/*$s*/';
			case CommentLine(s): '//$s';
			case IntInterval(s): '$s...';
			case Semicolon: ";";
			case Dot: ".";
			case DblDot: ":";
			case Arrow: "->";
			case Comma: ",";
			case BkOpen: "[";
			case BkClose: "]";
			case BrOpen: "{";
			case BrClose: "}";
			case POpen: "(";
			case PClose: ")";
			case Question: "?";
			case At: "@";
			case Eof: "<eof>";
		}
	}

	@:deprecated("Use toString() instead")
	static public inline function print(def:TokenDef) {
		return toString(def);
	}
}

class Token {
	public var tok: TokenDef;
	public var pos: Position;
	#if keep_whitespace
	public var space = "";
	#end
	public function new(tok, pos) {
		this.tok = tok;
		this.pos = pos;
	}

	public function toString() {
		return TokenDefPrinter.toString(tok);
	}
}

typedef EnumConstructor = {
	name : String,
	doc: String,
	meta: Metadata,
	args: Array<{ name: String, opt: Bool, type: ComplexType}>,
	pos: Position,
	params: Array<TypeParamDecl>,
	type: Null<ComplexType>
}

typedef Definition<A,B> = {
	name : String,
	doc: String,
	params: Array<TypeParamDecl>,
	meta: Metadata,
	flags: Array<A>,
	data: B
}

enum TypeDef {
	EClass(d:Definition<ClassFlag, Array<Field>>);
	EEnum(d:Definition<EnumFlag, Array<EnumConstructor>>);
	EAbstract(a:Definition<AbstractFlag, Array<Field>>);
	EImport(sl:Array<{pack:String, pos:Position}>, mode:ImportMode);
	ETypedef(d:Definition<EnumFlag, ComplexType>);
	EUsing(path:TypePath);
}

typedef TypeDecl = {
	decl : TypeDef,
	pos : Position
}

enum ClassFlag {
	HInterface;
	HExtern;
	HPrivate;
	HExtends(t:TypePath);
	HImplements(t:TypePath);
}

enum AbstractFlag {
	APrivAbstract;
	AFromType(ct:ComplexType);
	AToType(ct:ComplexType);
	AIsType(ct:ComplexType);
	AExtern;
}

enum EnumFlag {
	EPrivate;
	EExtern;
}

enum ImportMode {
	INormal;
	IAsName(s:String);
	IAll;
}
