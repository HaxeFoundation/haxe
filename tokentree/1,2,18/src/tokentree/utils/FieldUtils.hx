package tokentree.utils;

using Lambda;
using tokentree.utils.TokenTreeCheckUtils;

class FieldUtils {
	public static function getFieldType(field:Null<TokenTree>, defaultVisibility:TokenFieldVisibility):TokenFieldType {
		if (field == null) {
			return Unknown;
		}
		switch (field.tok) {
			case Kwd(KwdFunction):
				return getFunctionFieldType(field, defaultVisibility);
			case Kwd(KwdVar), Kwd(KwdFinal):
				return getVarFieldType(field, defaultVisibility);
			default:
		}
		return Unknown;
	}

	static function getFunctionFieldType(field:TokenTree, defaultVisibility:TokenFieldVisibility):TokenFieldType {
		var access:TokenTreeAccessHelper = TokenTreeAccessHelper.access(field).firstChild();
		if (access.token == null) {
			return Unknown;
		}
		var name = field.getNameToken().getName();
		var visibility:TokenFieldVisibility = defaultVisibility;
		var isStatic:Bool = false;
		var isInline:Bool = false;
		var isOverride:Bool = false;
		var isFinal:Bool = false;
		var isExtern:Bool = false;
		if (access.token.children != null) {
			for (child in access.token.children) {
				switch (child.tok) {
					case Kwd(KwdPublic):
						visibility = Public;
					case Kwd(KwdPrivate):
						visibility = Private;
					case Kwd(KwdStatic):
						isStatic = true;
					case Kwd(KwdInline):
						isInline = true;
					case Kwd(KwdOverride):
						isOverride = true;
					case Kwd(KwdExtern):
						isExtern = true;
					case Kwd(KwdFinal):
						isFinal = true;
					case POpen, BrOpen:
						break;
					default:
				}
			}
		}
		return Function(name, visibility, isStatic, isInline, isOverride, isFinal, isExtern);
	}

	static function getVarFieldType(field:TokenTree, defaultVisibility:TokenFieldVisibility):TokenFieldType {
		var access:TokenTreeAccessHelper = TokenTreeAccessHelper.access(field).firstChild();
		if (access.token == null) {
			return Unknown;
		}
		var name = field.getNameToken().getName();
		var visibility:TokenFieldVisibility = defaultVisibility;
		var isStatic:Bool = false;
		var isInline:Bool = false;
		var isFinal:Bool = field.tok.match(Kwd(KwdFinal));
		var isExtern:Bool = false;
		if (access.token.children != null) {
			for (child in access.token.children) {
				switch (child.tok) {
					case Kwd(KwdPublic):
						visibility = Public;
					case Kwd(KwdPrivate):
						visibility = Private;
					case Kwd(KwdStatic):
						isStatic = true;
					case Kwd(KwdInline):
						isInline = true;
					case Kwd(KwdExtern):
						isExtern = true;
					default:
				}
			}
		}
		access = access.firstOf(POpen);
		if (isFinal || access.token == null) {
			return Var(name, visibility, isStatic, isInline, isFinal, isExtern);
		}
		var getterAccess:TokenPropertyAccess = makePropertyAccess(access.firstChild().token);
		var setterAccess:TokenPropertyAccess = makePropertyAccess(access.child(1).token);
		return Prop(name, visibility, isStatic, getterAccess, setterAccess);
	}

	static function makePropertyAccess(accessToken:TokenTree):TokenPropertyAccess {
		if (accessToken == null) {
			return Default;
		}
		return switch (accessToken.tok) {
			case Kwd(KwdDefault): Default;
			case Kwd(KwdNull): NullAccess;
			case Kwd(KwdDynamic): DynamicAccess;
			case Const(CIdent("never")): Never;
			case Const(CIdent("get")): Get;
			case Const(CIdent("set")): Set;
			default: Default;
		}
	}

	public static function isOperatorFunction(functionToken:TokenTree):Bool {
		return functionToken.getMetadata().exists(function(meta) {
			return switch (meta.tok) {
				case Const(CIdent("op")): true;
				case Const(CIdent("arrayAccess")): true;
				case Const(CIdent("resolve")): true;
				case _: false;
			}
		});
	}
}

enum TokenFieldType {
	Function(name:String, visibility:TokenFieldVisibility, isStatic:Bool, isInline:Bool, isOverride:Bool, isFinal:Bool, isExtern:Bool);
	Var(name:String, visibility:TokenFieldVisibility, isStatic:Bool, isInline:Bool, isFinal:Bool, isExtern:Bool);
	Prop(name:String, visibility:TokenFieldVisibility, isStatic:Bool, getter:TokenPropertyAccess, setter:TokenPropertyAccess);
	Unknown;
}

enum TokenFieldVisibility {
	Public;
	Private;
}

enum TokenPropertyAccess {
	Default;
	NullAccess;
	Get;
	Set;
	DynamicAccess;
	Never;
}