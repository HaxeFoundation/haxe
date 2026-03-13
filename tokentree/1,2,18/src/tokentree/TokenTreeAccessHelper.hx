package tokentree;

abstract TokenTreeAccessHelper(Null<TokenTree>) from TokenTree {
	public var token(get, never):TokenTree;

	inline function get_token():TokenTree {
		return this;
	}

	public static inline function access(tok:TokenTree):TokenTreeAccessHelper {
		return tok;
	}

	public function parent():Null<TokenTreeAccessHelper> {
		return if (exists()) this.parent else null;
	}

	public function findParent(predicate:TokenTreeAccessHelper->Bool):Null<TokenTreeAccessHelper> {
		var parent:Null<TokenTreeAccessHelper> = parent();
		while (parent.exists() && parent.token.tok != Root) {
			if (predicate(parent)) {
				return parent;
			}
			parent = parent.parent();
		}
		return null;
	}

	public function previousSibling():Null<TokenTreeAccessHelper> {
		return if (exists()) this.previousSibling else null;
	}

	public function nextSibling():Null<TokenTreeAccessHelper> {
		return if (exists()) this.nextSibling else null;
	}

	public function firstChild():Null<TokenTreeAccessHelper> {
		return if (exists()) this.getFirstChild() else null;
	}

	public function lastChild():Null<TokenTreeAccessHelper> {
		return if (exists()) this.getLastChild() else null;
	}

	public function firstOf(token:TokenTreeDef):Null<TokenTreeAccessHelper> {
		if (!exists() || this.children == null) return null;
		for (tok in this.children) {
			if (Type.enumEq(tok.tok, token)) return tok;
		}
		return null;
	}

	public function lastOf(token:TokenTreeDef):Null<TokenTreeAccessHelper> {
		if (!exists() || this.children == null) return null;
		var found:Null<TokenTree> = null;
		for (tok in this.children) {
			if (Type.enumEq(tok.tok, token)) found = tok;
		}
		return found;
	}

	public function child(index:Int):Null<TokenTreeAccessHelper> {
		return if (exists() && this.children != null) this.children[index] else null;
	}

	public function matches(tok:TokenTreeDef):Null<TokenTreeAccessHelper> {
		return if (exists() && Type.enumEq(this.tok, tok)) this else null;
	}

	public function isComment():Null<TokenTreeAccessHelper> {
		return if (exists() && this.isComment()) this else null;
	}

	public function isCIdent():Null<TokenTreeAccessHelper> {
		return if (exists() && this.isCIdent()) this else null;
	}

	public function or(other:TokenTree):TokenTree {
		return if (exists()) this else other;
	}

	public inline function exists():Bool {
		return this != null;
	}
}

typedef TokenTreeDefMatcher = TokenTreeDef->Bool;