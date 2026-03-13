package tokentree.utils;

import tokentree.TokenTree.FilterResult;
import tokentree.walk.WalkSharp.WalkSharpConsts;

using Lambda;

class TokenTreeCheckUtils {
	public static function isImportMult(token:TokenTree):Bool {
		return switch (token.tok) {
			case Binop(OpMult), Dot: isImport(token.parent);
			default: false;
		}
	}

	public static function isImport(token:Null<TokenTree>):Bool {
		var parent:Null<TokenTree> = token;
		while (parent != null) {
			if (parent.tok == Root) return false;
			switch (parent.tok) {
				case Kwd(KwdMacro):
				case Kwd(KwdExtern):
				case Const(CIdent(_)):
				case Dot:
				case Kwd(KwdIn) | Binop(OpIn):
				case Kwd(KwdImport):
					return true;
				case Kwd(KwdUsing):
					return true;
				default:
					return false;
			}
			parent = parent.parent;
		}
		return false;
	}

	public static function isTypeParameter(token:TokenTree):Bool {
		return switch (token.tok) {
			case Binop(OpGt): (token.access().parent().matches(Binop(OpLt)).token != null);
			case Binop(OpLt): (token.access().firstOf(Binop(OpGt)).token != null);
			default: false;
		}
	}

	public static function isOpGtTypedefExtension(token:TokenTree):Bool {
		return switch (token.tok) {
			case Binop(OpGt): (token.access().parent().matches(BrOpen).parent().matches(Binop(OpAssign)).parent().isCIdent().parent().matches(Kwd(KwdTypedef))
					.token != null);
			default: false;
		}
	}

	public static function filterOpSub(token:Null<TokenTree>):Bool {
		if (token == null) {
			return false;
		}
		if (!token.tok.match(Binop(OpSub))) return false;
		var prev:Null<TokenTree> = token.previousSibling;
		if (token.previousSibling == null) {
			prev = token.parent;
		}
		else {
			prev = getLastToken(token.previousSibling);
			if (prev == null) {
				return false;
			}
		}
		switch (prev.tok) {
			case Kwd(KwdIn) | Binop(OpIn):
				return true;
			case Binop(_):
				return true;
			case Spread:
				return true;
			case BkOpen:
				return true;
			case BkClose:
				return false;
			case BrOpen:
				return true;
			case BrClose:
				return true;
			case POpen:
				return true;
			case PClose:
				var pOpen:TokenTree = prev.parent;
				var type:POpenType = getPOpenType(pOpen);
				switch (type) {
					case At: return true;
					case Parameter: return true;
					case Call: return false;
					case SwitchCondition: return true;
					case WhileCondition: return true;
					case IfCondition: return true;
					case SharpCondition: return true;
					case Catch: return false;
					case ForLoop: return true;
					case Expression: return false;
				}
			case Comma:
				return true;
			case Arrow:
				return true;
			case Question:
				return true;
			case DblDot:
				return true;
			case Kwd(KwdElse):
				return true;
			case Kwd(KwdWhile):
				return true;
			case Kwd(KwdDo):
				return true;
			case Kwd(KwdReturn):
				return true;
			default:
				return false;
		}
	}

	public static function isUnaryLeftSided(tok:TokenTree):Bool {
		var child:TokenTree = tok.getFirstChild();

		if (child == null) return false;
		return switch (child.tok) {
			case Const(_): true;
			case POpen: true;
			case Kwd(KwdMacro): true;
			case Kwd(KwdThis): true;
			default: false;
		}
	}

	public static function isTernary(token:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		if (token.tok.match(DblDot)) {
			return isTernary(token.parent);
		}
		if (!token.tok.match(Question)) {
			return false;
		}
		if (token.access().firstOf(DblDot).token == null) {
			return false;
		}
		if (token.parent == null) {
			return false;
		}
		switch (token.parent.tok) {
			case POpen:
				var prev:TokenTree = token.previousSibling;
				if (prev == null) {
					return false;
				}
				var lastToken:TokenTree = getLastToken(prev);
				if (lastToken == null) {
					return false;
				}
				switch (lastToken.tok) {
					case Comma: return false;
					case Semicolon: return false;
					default: return true;
				}
			case Comma:
				return false;
			case Binop(_):
				return true;
			case Kwd(KwdVar), Kwd(KwdFunction):
				return false;
			case Kwd(KwdFinal):
				return false;
			case Sharp(_):
				return false;
			default:
				return true;
		}
	}

	public static function isTypeEnumAbstract(type:TokenTree):Bool {
		switch (type.tok) {
			case Kwd(KwdAbstract):
				var name:TokenTree = type.access().firstChild().token;
				if ((name == null) || (name.children == null) || (name.children.length <= 0)) {
					return false;
				}
				if (name.access().firstOf(Kwd(KwdEnum)).exists()) {
					return true;
				}
				for (child in name.children) {
					if (!child.tok.match(At)) {
						continue;
					}
					var enumTok = child.access().firstChild().matches(DblDot).firstChild().matches(Kwd(KwdEnum));
					if (!enumTok.exists()) {
						continue;
					}
					return true;
				}
			default:
		}
		return false;
	}

	/**
		Whether this is a `typedef` to an anonymous structure, rather than being a "plain type alias".
	**/
	public static function isTypeStructure(typedefToken:TokenTree):Bool {
		var afterAssign = typedefToken.access().firstChild().isCIdent().firstOf(Binop(OpAssign)).firstChild();
		return afterAssign.matches(BrOpen).exists() || afterAssign.isCIdent().firstOf(Binop(OpAnd)).exists();
	}

	public static function isTypeEnum(enumToken:TokenTree):Bool {
		if (!enumToken.tok.match(Kwd(KwdEnum))) {
			return false;
		}
		if (isTypeEnumAbstract(enumToken)) {
			return false;
		}
		if (enumToken.access().parent().matches(DblDot).parent().matches(At).exists()) {
			return false;
		}
		return true;
	}

	public static function isTypeMacroClass(classToken:TokenTree):Bool {
		return classToken.tok.match(Kwd(KwdClass)) && classToken.access().parent().matches(Kwd(KwdMacro)).exists();
	}

	public static function isBrOpenAnonTypeOrTypedef(token:TokenTree):Bool {
		switch (getBrOpenType(token)) {
			case Block:
				return false;
			case TypedefDecl:
				return true;
			case ObjectDecl:
				return false;
			case AnonType:
				return true;
			case Unknown:
				return false;
		}
	}

	/**
		Gets the name for a name token (`KwdNew`, `CIdent`).
	**/
	public static function getName(token:TokenTree):Null<String> {
		if (token == null) {
			return null;
		}
		return switch (token.tok) {
			case Const(CIdent(ident)): ident;
			case Kwd(KwdNew): "new";
			case _: null;
		}
	}

	/**
		Gets the name token of a type, var or function token, or a CIdent token itself.
	**/
	public static function getNameToken(token:TokenTree):Null<TokenTree> {
		if (isNameToken(token)) {
			return token;
		}
		var nameToken = token.access().firstChild();
		if (isNameToken(nameToken.token)) {
			return nameToken.token;
		}
		nameToken = nameToken.matches(Question).firstChild();
		if (isNameToken(nameToken.token)) {
			return nameToken.token;
		}
		return null;
	}

	/**
		Whether the token is a name token (`KwdNew`, `CIdent`).
	**/
	public static function isNameToken(token:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		return switch (token.tok) {
			case Const(CIdent(_)), Kwd(KwdNew): true;
			case _: false;
		}
	}

	/**
		Returns a list of metas (without At + DblDot) for a given decl keyword (`function`, `var`...).
	**/
	public static function getMetadata(declToken:TokenTree):Array<TokenTree> {
		var ident = declToken.access().firstChild().isCIdent().token;
		if (ident == null || !ident.hasChildren()) {
			return [];
		}

		return ident.children.map(function(token) {
			return token.access().matches(At).firstChild().matches(DblDot).firstChild().token;
		}).filter(function(token) {
			return token != null;
		});
	}

	/**
		Tries to get the doc comment for a given type or field.
	**/
	public static function getDocComment(declToken:TokenTree):Null<TokenTree> {
		var access = declToken.access();
		do {
			access = access.previousSibling();
			if (access.token == null) {
				return null;
			}
			switch (access.token.tok) {
				case Comment(_):
					return access.token;
				case CommentLine(_):
					continue;
				case _:
					return null;
			}
		}
		while (true);
		return null;
	}

	public static function isModifier(keyword:TokenTree):Bool {
		if (keyword == null) {
			return false;
		}
		return switch (keyword.tok) {
			case Kwd(KwdPublic): true;
			case Kwd(KwdPrivate): true;
			case Kwd(KwdStatic): true;
			case Kwd(KwdInline): true;
			case Kwd(KwdOverride): true;
			case Kwd(KwdExtern): true;
			case Kwd(KwdDynamic): true;
			case Kwd(KwdMacro): true;
			case _: false;
		}
	}

	public static function getBrOpenType(token:TokenTree):BrOpenType {
		if (token == null) {
			return Unknown;
		}
		if (token.tokenTypeCache.brOpenType != null) {
			return token.tokenTypeCache.brOpenType;
		}

		var type:BrOpenType = determineBrOpenType(token);
		token.tokenTypeCache.brOpenType = type;
		return type;
	}

	static function determineBrOpenType(token:TokenTree):BrOpenType {
		if (token == null) {
			return Unknown;
		}
		if (token.parent == null || token.parent.tok == Root) {
			return determinBrChildren(token);
		}
		switch (token.parent.tok) {
			case Binop(OpAssign):
				if (isInsideTypedef(token.parent)) {
					return TypedefDecl;
				}
				return determinBrChildren(token);
			case Binop(OpLt):
				return AnonType;
			case Binop(OpArrow):
				return determinBrChildren(token);
			case Binop(_):
				return ObjectDecl;
			case Kwd(KwdReturn):
				return determinBrChildren(token);
			case Question:
				if (isTernary(token.parent)) {
					return ObjectDecl;
				}
			case DblDot:
				if (isTernary(token.parent)) {
					return ObjectDecl;
				}
				var parent:TokenTree = token.parent.parent;
				switch (parent.tok) {
					case Const(CIdent(_)):
					case Const(CString(_)):
					case Kwd(KwdCase): return ObjectDecl;
					case Kwd(KwdDefault): return ObjectDecl;
					default: return AnonType;
				}
				parent = parent.parent;
				switch (parent.tok) {
					case Question: return AnonType;
					case Kwd(KwdVar): return AnonType;
					case Kwd(KwdFunction): return AnonType;
					case Kwd(KwdFinal): return AnonType;
					case BrOpen: return getBrOpenType(parent);
					case POpen: return AnonType;
					case Binop(OpLt): return AnonType;
					default: return ObjectDecl;
				}
			case POpen:
				var pOpenType:POpenType = getPOpenType(token.parent);
				switch (pOpenType) {
					case At: return ObjectDecl;
					case Parameter: return AnonType;
					case Call: return ObjectDecl;
					case SwitchCondition: return Unknown;
					case WhileCondition: return Unknown;
					case IfCondition: return Unknown;
					case SharpCondition: return Unknown;
					case Catch: return Unknown;
					case ForLoop: return Unknown;
					case Expression: return ObjectDecl;
				}
			case BkOpen:
				return ObjectDecl;
			case Const(CIdent("from")), Const(CIdent("to")):
				return AnonType;
			case Const(_):
				return Block;
			case Dollar(_):
				return Block;
			default:
		}
		return determinBrChildren(token);
	}

	static function determinBrChildren(token:TokenTree):BrOpenType {
		if ((token.children == null) || (token.children.length <= 0)) {
			switch (token.parent.tok) {
				case Kwd(_):
					return Block;
				default:
					return ObjectDecl;
			}
		}
		if (token.parent != null && token.parent.tok != Root) {
			if (token.children.length == 1) {
				switch (token.parent.tok) {
					case Kwd(_):
						return Block;
					default:
						return ObjectDecl;
				}
			}
			if ((token.children.length == 2) && token.getLastChild().tok.match(Semicolon)) {
				switch (token.parent.tok) {
					case Kwd(_):
						return Block;
					default:
						return ObjectDecl;
				}
			}
		}
		if (TokenTreeAccessHelper.access(token).firstOf(Arrow).exists()) {
			return AnonType;
		}
		if ((token.nextSibling != null) && (token.nextSibling.tok.match(Arrow))) {
			return AnonType;
		}
		var onlyComment:Bool = true;
		for (child in token.children) {
			switch (child.tok) {
				case Const(CIdent(_)), Const(CString(_)):
					if (!child.access().firstChild().matches(DblDot).exists()) {
						return Block;
					}
					onlyComment = false;
				case At:
				case Comment(_), CommentLine(_):
				case BrClose:
					if (onlyComment) {
						if (token.parent != null && token.parent.tok != Root) {
							switch (token.parent.tok) {
								case Kwd(_): return Block;
								default: return ObjectDecl;
							}
						}
						else {
							return ObjectDecl;
						}
					}
					return ObjectDecl;
				case Sharp(_):
				default:
					return Block;
			}
		}
		return ObjectDecl;
	}

	public static function getBkOpenType(token:TokenTree):BkOpenType {
		if (token == null) {
			return Unknown;
		}
		if (token.tokenTypeCache.bkOpenType != null) {
			return token.tokenTypeCache.bkOpenType;
		}

		var type:BkOpenType = determineBkOpenType(token);
		token.tokenTypeCache.bkOpenType = type;
		return type;
	}

	static function determineBkOpenType(token:TokenTree):BkOpenType {
		if (token == null) {
			return Unknown;
		}
		var parent:TokenTree = token.parent;
		if (parent == null || parent.tok == Root) {
			return determinBkChildren(token);
		}
		return switch (parent.tok) {
			case Kwd(KwdThis): ArrayAccess;
			case BkOpen | BrOpen | POpen: determinBkChildren(token);
			case DblDot: determinBkChildren(token);
			case Binop(_): determinBkChildren(token);
			case Kwd(_): determinBkChildren(token);
			case Sharp(_): determinBkChildren(token);
			default: ArrayAccess;
		}
	}

	static function determinBkChildren(token:TokenTree):BkOpenType {
		if (!token.hasChildren()) {
			return Unknown;
		}
		for (child in token.children) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
				case Kwd(KwdFor) | Kwd(KwdWhile):
					return Comprehension;
				default:
					break;
			}
		}
		var bkDepth:Int = -1;
		var childArrows:Array<TokenTree> = token.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Binop(OpArrow): if (bkDepth == 0) {
						FoundSkipSubtree;
					}
					else {
						SkipSubtree;
					}
				case BkOpen:
					bkDepth++;
					GoDeeper;
				case BkClose:
					bkDepth--;
					GoDeeper;
				default: GoDeeper;
			}
		});
		if (childArrows.length > 0) {
			return MapLiteral;
		}
		return ArrayLiteral;
	}

	public static function getPOpenType(token:TokenTree):POpenType {
		if (token == null) {
			return Expression;
		}
		switch (token.tok) {
			case POpen:
			case PClose:
				return getPOpenType(token.parent);
			default:
				return Expression;
		}
		if (token.tokenTypeCache.pOpenType != null) {
			return token.tokenTypeCache.pOpenType;
		}

		var type:POpenType = determinePOpenType(token);
		token.tokenTypeCache.pOpenType = type;
		return type;
	}

	public static function determinePOpenType(token:TokenTree):POpenType {
		var parent:TokenTree = token.parent;
		if ((parent == null) || (parent.tok == Root)) {
			return Expression;
		}
		if (hasAtParent(token)) {
			return At;
		}
		if (token.hasChildren() && checkPOpenForArrowChildren(token)) {
			return Parameter;
		}
		while ((parent != null) && (parent.tok != Root)) {
			switch (parent.tok) {
				case Binop(OpLt):
					parent = parent.parent;
				case Sharp(WalkSharpConsts.IF), Sharp(WalkSharpConsts.ELSEIF):
					if (parent.getFirstChild() == token) {
						return SharpCondition;
					}
					parent = parent.parent;
				default:
					break;
			}
		}
		if ((parent == null) || (parent.tok == Root)) {
			return Expression;
		}
		switch (parent.tok) {
			case Kwd(KwdIf):
				var firstChild:Null<TokenTree> = parent.getFirstChild();
				if (firstChild == null) {
					return IfCondition;
				}
				if (firstChild.index == token.index) {
					return IfCondition;
				}
				return Expression;
			case Kwd(KwdWhile):
				return WhileCondition;
			case Kwd(KwdSwitch):
				return SwitchCondition;
			case Kwd(KwdFor):
				return ForLoop;
			case Kwd(KwdCatch):
				return Catch;
			case POpen:
				return Expression;
			case Kwd(KwdFunction):
				return Parameter;
			case Kwd(KwdNew):
				return Parameter;
			case Const(CIdent(_)):
				if ((parent.parent == null) || (parent.parent.tok == Root)) {
					return Call;
				}
				switch (parent.parent.tok) {
					case Kwd(KwdFunction):
						if (parent.previousSibling == null) {
							var pOpen:Null<TokenTree> = parent.access().firstOf(POpen).token;
							if (pOpen == null) {
								return Parameter;
							}
							if (pOpen.index == token.index) {
								return Parameter;
							}
							return Expression;
						}
						return Call;
					case Kwd(KwdAbstract): return Parameter;
					case BrOpen:
						if (parent.parent.access().parent().parent().matches(Kwd(KwdEnum)).exists()) {
							return Parameter;
						}
						return Call;
					default: return Call;
				}
			default:
		}
		if (TokenTreeAccessHelper.access(token).firstOf(Arrow).exists()) {
			return Parameter;
		}
		return Expression;
	}

	static function checkPOpenForArrowChildren(token:TokenTree):Bool {
		var skip:Bool = true;
		for (child in token.children) {
			switch (child.tok) {
				case PClose:
					skip = false;
				default:
			}
			if (skip) {
				continue;
			}
			switch (child.tok) {
				case Arrow:
					return true;
				default:
			}
		}
		return false;
	}

	public static function hasAtParent(token:TokenTree):Bool {
		var parent:TokenTree = token.parent;
		while (parent.tok != Root) {
			switch (parent.tok) {
				case Const(_):
				case Dot:
				case DblDot:
				case At:
					return true;
				case Kwd(_):
				case Binop(OpIn):
				default:
					return false;
			}
			parent = parent.parent;
		}
		return false;
	}

	public static function isInsideTypedef(token:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		var parent:TokenTree = token;
		while (parent.parent != null) {
			if (parent.tok.match(Kwd(KwdTypedef))) {
				return true;
			}
			parent = parent.parent;
		}
		return false;
	}

	/**
		Whether a given type or field has `@:deprecated` metadata.
	 */
	public static function isDeprecated(declToken:TokenTree):Bool {
		return getMetadata(declToken).exists(function(meta) return meta.tok.match(Const(CIdent("deprecated"))));
	}

	public static function getArrowType(token:TokenTree):ArrowType {
		if (token == null) {
			return ArrowFunction;
		}
		if (token.tokenTypeCache.arrowType != null) {
			return token.tokenTypeCache.arrowType;
		}

		var type:Null<ArrowType> = determineArrowType(token);
		if (type == null) {
			type = ArrowFunction;
		}
		token.tokenTypeCache.arrowType = type;
		return type;
	}

	public static function determineArrowType(token:TokenTree):ArrowType {
		if (token == null) {
			return ArrowFunction;
		}
		var child:TokenTree = token.getFirstChild();
		while (child != null) {
			switch (child.tok) {
				case Arrow, Dot, Semicolon, Question:
				case BrOpen:
					var brClose:TokenTree = child.getFirstChild();
					if (brClose == null) {
						break;
					}
					if (brClose.tok.match(BrClose)) {
						return ArrowFunction;
					}
					var brType:Null<BrOpenType> = getBrOpenType(child);
					if (brType == null) {
						brType = Unknown;
					}
					switch (brType) {
						case Block: return ArrowFunction;
						case AnonType:
						default:
					}
					child = child.nextSibling;
					continue;
				case Const(CIdent(_)), Kwd(KwdMacro):
				case Binop(OpLt):
					child = child.nextSibling;
					continue;
				case POpen:
				case Comment(_), CommentLine(_):
				default:
					return ArrowFunction;
			}
			child = child.getFirstChild();
		}
		var parent:TokenTree = token.parent;
		if (parent == null) {
			return ArrowFunction;
		}
		var resultType:Null<ArrowType> = checkArrowParent(parent);
		if (resultType != null) {
			return resultType;
		}
		return checkArrowChildren(parent);
	}

	static function checkArrowChildren(parent:TokenTree):ArrowType {
		var child:TokenTree = parent.getFirstChild();

		if (child == null) {
			return ArrowFunction;
		}
		var seenArrow:Bool = false;
		while (child != null) {
			switch (child.tok) {
				case Arrow:
					seenArrow = true;
				case Const(CIdent(_)):
				case Kwd(_):
					return ArrowFunction;
				case Dot, Semicolon:
				case BrOpen, DblDot:
				case Binop(OpLt):
					child = child.nextSibling;
					continue;
				case POpen:
					var result:Null<ArrowType> = checkArrowPOpen(child);
					if (result != null) {
						return result;
					}
					child = child.nextSibling;
					continue;
				case PClose:
				case Question:
				case Comment(_), CommentLine(_):
				default:
					return NewFunctionType;
			}
			child = child.getFirstChild();
		}
		if (seenArrow) {
			return OldFunctionType;
		}
		return NewFunctionType;
	}

	static function checkArrowPOpen(token:TokenTree):Null<ArrowType> {
		if ((token.children == null) || (token.children.length <= 1)) {
			return null;
		}
		if (token.parent.isCIdent()) {
			return ArrowFunction;
		}
		var childArrows:Array<TokenTree> = token.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Arrow: FoundSkipSubtree;
				default: GoDeeper;
			}
		});
		if (childArrows.length <= 0) {
			return ArrowFunction;
		}
		var childArrows:Array<TokenTree> = token.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case DblDot: FoundSkipSubtree;
				default: GoDeeper;
			}
		});
		if (childArrows.length > 0) {
			return NewFunctionType;
		}
		return OldFunctionType;
	}

	static function checkArrowParent(parent:TokenTree):Null<ArrowType> {
		if (parent == null) {
			return ArrowFunction;
		}
		switch (parent.tok) {
			case POpen:
				switch (parent?.parent.tok) {
					case POpen: return ArrowFunction;
					default:
				}
			case Const(CIdent(_)):
				if ((parent.parent == null) || (parent.parent.tok == Root)) return ArrowFunction;
				switch (parent.parent.tok) {
					case POpen:
						var type:POpenType = getPOpenType(parent.parent);
						if (type == null) {
							type = Expression;
						}
						switch (type) {
							case Parameter: return OldFunctionType;
							case Expression: return OldFunctionType;
							default: return ArrowFunction;
						}
					case Binop(OpAssign):
						if (isInsideTypedef(parent.parent)) {
							return OldFunctionType;
						}
						return ArrowFunction;
					case Binop(OpArrow): return ArrowFunction;
					case Arrow: return OldFunctionType;
					case DblDot:
						var type:ColonType = getColonType(parent.parent);
						switch (type) {
							case TypeHint: return OldFunctionType;
							case TypeCheck: return OldFunctionType;
							case SwitchCase | Ternary | ObjectLiteral | At | Unknown: return ArrowFunction;
						}
					default:
				}
			default:
				return OldFunctionType;
		}
		return null;
	}

	public static function getColonType(token:TokenTree):ColonType {
		if (token == null) {
			return Unknown;
		}
		if (token.tokenTypeCache.colonType != null) {
			return token.tokenTypeCache.colonType;
		}

		var type:ColonType = determineColonType(token);
		token.tokenTypeCache.colonType = type;
		return type;
	}

	public static function determineColonType(token:TokenTree):ColonType {
		if (token == null) {
			return Unknown;
		}
		if (isTernary(token)) {
			return Ternary;
		}
		var parent:TokenTree = token.parent;
		if (parent == null) {
			return Unknown;
		}
		switch (parent.tok) {
			case Sharp(_):
				parent = parent.parent;
				if ((parent == null) || (parent.tok == Root)) {
					return Unknown;
				}
			default:
		}

		switch (parent.tok) {
			case Kwd(KwdCase), Kwd(KwdDefault):
				return SwitchCase;
			case At:
				return At;
			case BrOpen:
				var brClose:TokenTree = parent.access().firstOf(BrClose).token;
				if (brClose == null) {
					return Unknown;
				}
				if (brClose.pos.max <= token.pos.min) {
					return TypeCheck;
				}
			case POpen:
				return findColonParent(token);
			case Binop(OpLt):
				return findColonParent(parent);
			case Const(_):
				return findColonParent(parent);
			case Kwd(KwdNew):
				return findColonParent(parent);
			case Kwd(KwdThis):
				return findColonParent(parent);
			case Question:
				return findColonParent(parent);
			case Kwd(KwdFunction):
				return TypeHint;
			case Kwd(KwdMacro):
				return TypeHint;
			default:
		}
		return Unknown;
	}

	static function findColonParent(token:TokenTree):ColonType {
		var parent:TokenTree = token;
		while (parent.tok != Root) {
			switch (parent.tok) {
				case Kwd(KwdFunction) | Kwd(KwdVar) | Kwd(KwdFinal):
					return TypeHint;
				case BrOpen:
					var brType:BrOpenType = getBrOpenType(parent);
					switch (brType) {
						case Block: return Unknown;
						case TypedefDecl: return TypeHint;
						case ObjectDecl: return ObjectLiteral;
						case AnonType: return TypeHint;
						case Unknown: return Unknown;
					}
				case POpen:
					var pClose:TokenTree = parent.access().firstOf(PClose).token;
					if ((pClose != null) && (pClose.pos.max <= token.pos.min)) {
						return TypeCheck;
					}
					var pType:POpenType = getPOpenType(parent);
					switch (pType) {
						case At: return ObjectLiteral;
						case Parameter: return TypeHint;
						case Call: return Unknown;
						case SwitchCondition: return TypeCheck;
						case WhileCondition: return Unknown;
						case IfCondition: return Unknown;
						case SharpCondition: return Unknown;
						case Catch: return Unknown;
						case ForLoop: return TypeCheck;
						case Expression: return TypeCheck;
					}
				default:
			}
			parent = parent.parent;
		}
		return Unknown;
	}

	public static function getLastToken(token:TokenTree):Null<TokenTree> {
		if (token == null) {
			return null;
		}
		if (token.children == null) {
			return token;
		}
		if (token.children.length <= 0) {
			return token;
		}
		var lastChild:TokenTree = token.getLastChild();
		while (lastChild != null) {
			var newLast:TokenTree = lastChild.getLastChild();
			if (newLast == null) {
				return lastChild;
			}
			lastChild = newLast;
		}
		return null;
	}

	public static function isMetadata(token:Null<TokenTree>):Bool {
		if ((token == null) || (token.tok == Root)) {
			return false;
		}
		var parent:Null<TokenTree> = token.parent;
		while ((parent != null) && (parent.tok != Root)) {
			switch (parent.tok) {
				case DblDot:
					parent = parent.parent;
					if ((parent == null) || (parent.tok == Root)) {
						return false;
					}
					switch (parent.tok) {
						case At: return true;
						default: return false;
					}
				case At:
					return true;
				default:
					parent = parent.parent;
			}
		}
		return false;
	}

	public static function findLastBinop(token:Null<TokenTree>):Null<TokenTree> {
		if (token == null) {
			return null;
		}
		var lastBinop:Null<TokenTree> = null;
		function visitSiblings(parent:TokenTree):Null<TokenTree> {
			if (parent == null) {
				return null;
			}
			while (parent.nextSibling != null) {
				switch (parent.nextSibling.tok) {
					case Binop(_):
						parent = parent.nextSibling;
						lastBinop = parent;
					case Dot:
						return parent.nextSibling;
					case Comma | Semicolon:
						return parent;
					default:
						return parent;
				}
			}
			return parent;
		}

		token = visitSiblings(token);

		while (token.hasChildren()) {
			var child = switch (token.tok) {
				case BkOpen: token.access().firstOf(BkClose).token;
				case BrOpen: token.access().firstOf(BrClose).token;
				case POpen:
					token.access().firstOf(PClose).token;
					token = visitSiblings(token);
				default: token.getFirstChild();
			}
			if (child == null) {
				return lastBinop;
			}
			switch (child.tok) {
				case Binop(_):
					lastBinop = child;
					token = visitSiblings(child);
				case BkOpen:
					token = child.access().firstOf(BkClose).token;
					if (token == null) {
						return lastBinop;
					}
				case BrOpen:
					token = child.access().firstOf(BrClose).token;
					if (token == null) {
						return lastBinop;
					}
				case POpen:
					token = child.access().firstOf(PClose).token;
					if (token == null) {
						return lastBinop;
					}
					token = visitSiblings(token);
				case Dot:
					token = visitSiblings(child);
				default:
					token = child;
			}
		}
		return lastBinop;
	}
}

enum BrOpenType {
	Block;
	TypedefDecl;
	ObjectDecl;
	AnonType;
	Unknown;
}

enum BkOpenType {
	ArrayAccess;
	ArrayLiteral;
	Comprehension;
	MapLiteral;
	Unknown;
}

enum POpenType {
	At;
	Parameter;
	Call;
	SwitchCondition;
	WhileCondition;
	IfCondition;
	SharpCondition;
	Catch;
	ForLoop;
	Expression;
}

enum ArrowType {
	ArrowFunction;
	OldFunctionType;
	NewFunctionType;
}

enum ColonType {
	SwitchCase;
	TypeHint;
	TypeCheck;
	Ternary;
	ObjectLiteral;
	At;
	Unknown;
}