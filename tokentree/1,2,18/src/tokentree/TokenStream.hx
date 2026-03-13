package tokentree;

import byte.ByteData;
import haxeparser.Data;

class TokenStream {
	public static inline var NO_MORE_TOKENS:String = "no more tokens";
	public static var MODE:TokenStreamMode = Strict;

	var tokens:Array<Token>;
	var current:Int;
	var bytes:ByteData;
	var sharpIfStack:Array<TokenTree>;
	var tempStore:Array<TokenTree>;

	public function new(tokens:Array<Token>, bytes:ByteData) {
		this.tokens = tokens;
		this.bytes = bytes;
		sharpIfStack = [];
		tempStore = [];
		current = 0;
	}

	public function hasMore():Bool {
		return current < tokens.length;
	}

	public function consumeToken():TokenTree {
		if ((current < 0) || (current >= tokens.length)) {
			switch (MODE) {
				case Relaxed:
					return createDummyToken(CommentLine("auto insert"));
				case Strict:
					throw NO_MORE_TOKENS;
			}
		}
		var token:Token = tokens[current];
		current++;
		#if debugTokenTree
		Sys.println(token);
		#end
		var space:String = "";
		#if keep_whitespace
		space = token.space;
		#end
		return new TokenTree(ToTokenTreeDef.fromTokenDef(token.tok), space, token.pos, current - 1);
	}

	public function consumeConstIdent():TokenTree {
		switch (token()) {
			case Dollar(_):
				return consumeToken();
			case Const(CIdent(_)):
				return consumeToken();
			case Kwd(KwdOperator):
				// pre 4.0.0 code might still use `operator` as an identifier
				return consumeToken();
			default:
				switch (MODE) {
					case Relaxed: return createDummyToken(Const(CIdent("autoInsert")));
					case Strict: error('bad token ${token()} != Const(CIdent(_))');
				}
		}
	}

	public function consumeConst():TokenTree {
		switch (token()) {
			case Const(_):
				return consumeToken();
			default:
				switch (MODE) {
					case Relaxed: return createDummyToken(Const(CString("autoInsert")));
					case Strict: error('bad token ${token()} != Const(_)');
				}
		}
	}

	public function consumeTokenDef(tokenDef:TokenTreeDef):TokenTree {
		if (matches(tokenDef)) return consumeToken();
		switch (MODE) {
			case Relaxed:
				return createDummyToken(tokenDef);
			case Strict:
				error('bad token ${token()} != $tokenDef');
		}
	}

	public function consumeToTempStore() {
		tempStore.push(consumeToken());
	}

	public function addToTempStore(token:TokenTree) {
		tempStore.push(token);
	}

	public function applyTempStore(parent:TokenTree) {
		while (tempStore.length > 0) {
			parent.addChild(tempStore.shift());
		}
	}

	public function hasTempStore():Bool {
		return tempStore.length > 0;
	}

	public function getTempStore():Array<TokenTree> {
		return tempStore;
	}

	public function clearTempStore() {
		tempStore = [];
	}

	public inline function error(s:String) {
		throw formatCurrentPos() + ": " + s;
	}

	function formatCurrentPos():String {
		var pos = tokens[current].pos;
		return new hxparse.Position(pos.file, pos.min, pos.max).format(bytes);
	}

	public function matches(tokenDef:TokenTreeDef):Bool {
		if ((current < 0) || (current >= tokens.length)) return false;
		var token:Token = tokens[current];
		return Type.enumEq(tokenDef, ToTokenTreeDef.fromTokenDef(token.tok));
	}

	public function isSharp():Bool {
		if ((current < 0) || (current >= tokens.length)) return false;
		var token:Token = tokens[current];
		return switch (token.tok) {
			case Sharp(_): true;
			default: false;
		}
	}

	public function isTypedParam():Bool {
		if ((current < 0) || (current >= tokens.length)) return false;
		var index:Int = current + 1;
		var token:Token = tokens[current];
		switch (token.tok) {
			case Binop(OpLt):
			default:
				return false;
		}
		var depth:Int = 1;
		var brDepth:Int = 0;
		var bkDepth:Int = 0;
		var pDepth:Int = 0;
		while (true) {
			token = tokens[index++];
			switch (token.tok) {
				case Dot | DblDot | Comma | Arrow | Const(_) | Dollar(_):
				case POpen:
					pDepth++;
				case PClose:
					if (pDepth <= 0) {
						return false;
					}
					pDepth--;
				case BrOpen:
					brDepth++;
				case BrClose:
					if (brDepth <= 0) {
						return false;
					}
					brDepth--;
				case BkOpen:
					bkDepth++;
				case BkClose:
					if (bkDepth <= 0) {
						return false;
					}
					bkDepth--;
				case Binop(OpLt):
					if ((pDepth > 0) || (bkDepth > 0) || (brDepth > 0)) {
						continue;
					}
					depth++;
				case Binop(OpGt):
					if ((pDepth > 0) || (bkDepth > 0) || (brDepth > 0)) {
						continue;
					}
					depth--;
					if (depth <= 0) {
						return true;
					}
				default:
					return false;
			}
			if (index >= tokens.length) {
				return false;
			}
		}
		return false;
	}

	public function token():TokenTreeDef {
		if ((current < 0) || (current >= tokens.length)) {
			switch (MODE) {
				case Relaxed:
					return CommentLine("auto insert");
				case Strict:
					throw NO_MORE_TOKENS;
			}
		}
		return ToTokenTreeDef.fromTokenDef(tokens[current].tok);
	}

	public function tokenForMatch():TokenTreeDef {
		if ((current < 0) || (current >= tokens.length)) {
			return Root;
		}
		return ToTokenTreeDef.fromTokenDef(tokens[current].tok);
	}

	public function peekNonCommentToken():Null<TokenTreeDef> {
		if ((current < 0) || (current >= tokens.length)) {
			switch (MODE) {
				case Relaxed:
					return Const(CString("auto insert"));
				case Strict:
					throw NO_MORE_TOKENS;
			}
		}
		var index:Int = current;
		while (index < tokens.length) {
			var token:Token = tokens[index++];
			if (Token == null) {
				continue;
			}
			switch (token.tok) {
				case Comment(_), CommentLine(_):
				default:
					return ToTokenTreeDef.fromTokenDef(token.tok);
			}
		}
		return Root;
	}

	public function getTokenPos():Null<Position> {
		if ((current < 0) || (current >= tokens.length)) {
			return null;
		}
		return tokens[current].pos;
	}

	public function rewind() {
		if (current <= 0) return;
		current--;
	}

	public function getStreamIndex():Int {
		return current;
	}

	public function rewindTo(pos:Int) {
		current = pos;
	}

	/**
	 * HaxeLexer does not handle '>=', '>>', '>>=' and '>>>=' it produces an
	 * individual token for each character.
	 * This function provides a workaround, which scans the tokens following a
	 * Binop(OpGt) and if it is followed by Binop(OpAssign) or Binop(OpGt),
	 * it returns the correct token:
	 * '>' -> Binop(OpGt)
	 * '>=' -> Binop(OpGte)
	 * '>>' -> Binop(OpShr)
	 * '>>=' -> Binop(OpAssignOp(OpShr))
	 * '>>>=' -> Binop(OpAssignOp(OpUShr))
	 *
	 */
	public function consumeOpGt():TokenTree {
		var tok:TokenTree = consumeTokenDef(Binop(OpGt));
		switch (token()) {
			case Binop(OpGt):
				return consumeOpShr(tok);
			case Binop(OpAssign):
				var assignTok:TokenTree = consumeToken();
				return new TokenTree(Binop(OpGte), tok.space + assignTok.space, {file: tok.pos.file, min: tok.pos.min, max: assignTok.pos.max}, tok.index);
			default:
				return tok;
		}
	}

	function consumeOpShr(parent:TokenTree):TokenTree {
		var tok:TokenTree = consumeTokenDef(Binop(OpGt));
		switch (token()) {
			case Binop(OpGt):
				var innerGt:TokenTree = consumeToken();
				switch (token()) {
					case Binop(OpAssign):
						var assignTok:TokenTree = consumeToken();
						return new TokenTree(Binop(OpAssignOp(OpUShr)), assignTok.space, {
							file: parent.pos.file,
							min: parent.pos.min,
							max: assignTok.pos.max
						}, parent.index);
					default:
				}
				return new TokenTree(Binop(OpUShr), innerGt.space, {file: parent.pos.file, min: parent.pos.min, max: innerGt.pos.max}, parent.index);
			case Binop(OpAssign):
				var assignTok:TokenTree = consumeToken();
				return new TokenTree(Binop(OpAssignOp(OpShr)), assignTok.space, {
					file: parent.pos.file,
					min: parent.pos.min,
					max: assignTok.pos.max
				}, parent.index);
			default:
				return new TokenTree(Binop(OpShr), tok.space, {file: parent.pos.file, min: parent.pos.min, max: tok.pos.max}, parent.index);
		}
	}

	/**
	 * HaxeLexer does not detect negative Const(CInt(_)) or Const(CFloat(_))
	 * This function provides a workaround, which scans the tokens around
	 * Binop(OpSub) to see if the token stream should contain a negative const
	 * value and returns a proper Const(CInt(-x)) or Const(CFloat(-x)) token
	 */
	public function consumeOpSub(parent:TokenTree):TokenTree {
		var tok:TokenTree = consumeTokenDef(Binop(OpSub));
		switch (token()) {
			case Const(CInt(_)) | Const(CFloat(_)):
			default:
				return new TokenTree(tok.tok, tok.space, tok.pos, tok.index);
		}
		var previous:Int = current - 2;
		if (previous < 0) throw NO_MORE_TOKENS;
		var prevTok:Token = tokens[previous];
		switch (prevTok.tok) {
			case Binop(_) | Unop(_) | BrOpen | BkOpen | POpen | Comma | DblDot | IntInterval(_) | Question | Semicolon:
			case Kwd(KwdReturn) | Kwd(KwdIf) | Kwd(KwdElse) | Kwd(KwdWhile) | Kwd(KwdDo) | Kwd(KwdFor) | Kwd(KwdCase) | Kwd(KwdCast):
			case PClose:
				if ((parent == null) || (parent.tok == Root)) {
					return new TokenTree(tok.tok, tok.space, tok.pos, tok.index);
				}
				switch (parent.tok) {
					case Kwd(KwdIf) | Kwd(KwdElse) | Kwd(KwdWhile) | Kwd(KwdDo) | Kwd(KwdFor) | Kwd(KwdCatch):
					default: return new TokenTree(tok.tok, tok.space, tok.pos, tok.index);
				}
			default:
				return new TokenTree(tok.tok, tok.space, tok.pos, tok.index);
		}
		switch (token()) {
			#if (haxe >= version("4.3.0-rc.1"))
			case Const(CInt(v, s)):
				var const:TokenTree = consumeConst();
				return new TokenTree(Const(CInt('-$v', s)), const.space, {file: tok.pos.file, min: tok.pos.min, max: const.pos.max}, tok.index);
			case Const(CFloat(v, s)):
				var const:TokenTree = consumeConst();
				return new TokenTree(Const(CFloat('-$v', s)), const.space, {file: tok.pos.file, min: tok.pos.min, max: const.pos.max}, tok.index);
			#else
			case Const(CInt(n)):
				var const:TokenTree = consumeConst();
				return new TokenTree(Const(CInt('-$n')), const.space, {file: tok.pos.file, min: tok.pos.min, max: const.pos.max}, tok.index);
			case Const(CFloat(n)):
				var const:TokenTree = consumeConst();
				return new TokenTree(Const(CFloat('-$n')), const.space, {file: tok.pos.file, min: tok.pos.min, max: const.pos.max}, tok.index);
			#end
			default:
				throw NO_MORE_TOKENS;
		}
	}

	public function pushSharpIf(token:TokenTree) {
		sharpIfStack.push(token);
	}

	public function popSharpIf():TokenTree {
		var token:Null<TokenTree> = sharpIfStack.pop();
		if (token == null) {
			switch (MODE) {
				case Relaxed:
					return createDummyToken(CommentLine("dummy token"));
				case Strict:
					throw NO_MORE_TOKENS;
			}
		}
		return @:nullSafety(Off) token;
	}

	public function peekSharpIf():TokenTree {
		if (sharpIfStack.length <= 0) {
			switch (MODE) {
				case Relaxed:
					return createDummyToken(CommentLine("dummy token"));
				case Strict:
					throw NO_MORE_TOKENS;
			}
		}
		return sharpIfStack[sharpIfStack.length - 1];
	}

	public function createDummyToken(tokDef:TokenTreeDef):TokenTree {
		var pos:Position;
		if (tokens.length <= 0) {
			return new TokenTree(tokDef, "", {file: "<unknown>", min: 0, max: 0}, -1, true);
		}
		if ((current < 0) || (current >= tokens.length)) {
			var prevPos:Position = tokens[tokens.length - 1].pos;
			pos = {
				min: prevPos.max,
				max: prevPos.max,
				file: prevPos.file
			}
		}
		else {
			var prevPos:Position = tokens[current].pos;
			pos = {
				min: prevPos.min,
				max: prevPos.min,
				file: prevPos.file
			}
		}
		return new TokenTree(tokDef, "", pos, -1, true);
	}
}

enum TokenStreamMode {
	Strict;
	Relaxed;
}