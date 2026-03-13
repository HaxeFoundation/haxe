package tokentree;

import tokentree.utils.TokenTreeCheckUtils.ArrowType;
import tokentree.utils.TokenTreeCheckUtils.BkOpenType;
import tokentree.utils.TokenTreeCheckUtils.BrOpenType;
import tokentree.utils.TokenTreeCheckUtils.ColonType;
import tokentree.utils.TokenTreeCheckUtils.POpenType;

class TokenTree {
	static inline var MAX_LEVEL:Int = 9999;

	public var tok:TokenTreeDef;
	public var pos:Position;
	public var parent:Null<TokenTree>;
	public var previousSibling:Null<TokenTree>;
	public var nextSibling:Null<TokenTree>;
	public var children:Null<Array<TokenTree>>;
	public var index:Int;
	public var inserted:Bool;
	public var space:String;

	@:allow(tokentree.utils.TokenTreeCheckUtils)
	var tokenTypeCache:TokenTypeCache;

	public function new(tok:TokenTreeDef, space:String, pos:Position, index:Int, inserted:Bool = false) {
		this.tok = tok;
		this.pos = pos;
		this.index = index;
		this.inserted = inserted;
		this.space = space;
		tokenTypeCache = {};
	}

	public function matches(tokenDef:TokenTreeDef):Bool {
		return Type.enumEq(tokenDef, tok);
	}

	public function isComment():Bool {
		return switch (tok) {
			case Comment(_), CommentLine(_): true;
			default: false;
		}
	}

	public function isCIdent():Bool {
		return switch (tok) {
			case Const(CIdent(_)): true;
			default: false;
		}
	}

	public function isCIdentOrCString():Bool {
		return switch (tok) {
			case Const(CIdent(_)): true;
			case Const(CString(_)): true;
			default: false;
		}
	}

	public function addChild(child:Null<TokenTree>) {
		if (child == null) return;
		if (children == null) children = [];
		if (children.length > 0) {
			child.previousSibling = children[children.length - 1];
			children[children.length - 1].nextSibling = child;
		}
		children.push(child);
		child.parent = this;
	}

	public function hasChildren():Bool {
		if (children == null) return false;
		return children.length > 0;
	}

	public function getFirstChild():Null<TokenTree> {
		if (!hasChildren()) return null;
		return @:nullSafety(Off) children[0];
	}

	public function getLastChild():Null<TokenTree> {
		if (!hasChildren()) return null;
		return @:nullSafety(Off) children[children.length - 1];
	}

	public function getPos():Position {
		if ((children == null) || (children.length <= 0)) return pos;
		var fullPos:Position = {file: pos.file, min: pos.min, max: pos.max};
		var childPos:Position;
		for (child in children) {
			childPos = child.getPos();
			if (childPos.min < fullPos.min) fullPos.min = childPos.min;
			if (childPos.max > fullPos.max) fullPos.max = childPos.max;
		}
		return fullPos;
	}

	public function filter(searchFor:Array<TokenTreeDef>, mode:TokenFilterMode, maxLevel:Int = MAX_LEVEL):Array<TokenTree> {
		return filterCallback(function(token:TokenTree, depth:Int):FilterResult {
			if (depth > maxLevel) return SkipSubtree;
			if (token.matchesAny(searchFor)) {
				if (mode == All) return FoundGoDeeper;
				return FoundSkipSubtree;
			}
			else return GoDeeper;
		});
	}

	public function filterCallback(callback:FilterCallback):Array<TokenTree> {
		var results:Array<TokenTree> = [];
		internalFilterCallback(callback, results, 0);
		return results;
	}

	function internalFilterCallback(callback:FilterCallback, results:Array<TokenTree>, depth:Int = 0) {
		switch (tok) {
			case Root:
			default:
				switch (callback(this, depth)) {
					case FoundGoDeeper: results.push(this);
					case FoundSkipSubtree:
						results.push(this);
						return;
					case GoDeeper:
					case SkipSubtree: return;
				}
		}
		if (children == null) return;
		for (child in children) {
			switch (child.tok) {
				case Sharp(_):
					child.internalFilterCallback(callback, results, depth);
				default:
					child.internalFilterCallback(callback, results, depth + 1);
			}
		}
	}

	function matchesAny(searchFor:Array<TokenTreeDef>):Bool {
		if (searchFor == null || tok == Root) return false;
		for (search in searchFor) {
			if (Type.enumEq(tok, search)) return true;
		}
		return false;
	}

	public function printTokenTree(prefix:String = ""):String {
		var buf:StringBuf = new StringBuf();
		var tokString:String = '$tok';
		if (inserted) tokString = '*** $tokString ***';
		if (tok != Root) buf.add('$prefix$tokString\t\t\t\t${getPos()}');
		if (children == null) return buf.toString();
		for (child in children) buf.add('\n$prefix${child.printTokenTree(prefix + "  ")}');
		return buf.toString();
	}

	public function toString():String {
		return TokenTreeDefPrinter.toString(tok);
	}
}

enum TokenFilterMode {
	All;
	First;
}

typedef FilterCallback = TokenTree->Int->FilterResult;

enum FilterResult {
	FoundSkipSubtree;
	FoundGoDeeper;
	SkipSubtree;
	GoDeeper;
}

typedef TokenTypeCache = {
	@:optional var brOpenType:Null<BrOpenType>;
	@:optional var bkOpenType:Null<BkOpenType>;
	@:optional var pOpenType:Null<POpenType>;
	@:optional var colonType:Null<ColonType>;
	@:optional var arrowType:Null<ArrowType>;
}