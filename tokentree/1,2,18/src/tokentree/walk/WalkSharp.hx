package tokentree.walk;

class WalkSharp {
	/**
	 * Sharp("if") | Sharp("elseif")
	 *  |- POpen
	 *      |- expression
	 *      |- PClose
	 *
	 * Sharp("if") | Sharp("elseif")
	 *  |- expression
	 *
	 * Sharp("end")
	 *
	 * Sharp("else")
	 *
	 * Sharp(_)
	 *
	 */
	public static function walkSharp(stream:TokenStream, parent:TokenTree, walker:WalkCB) {
		switch (stream.token()) {
			case Sharp(IF):
				WalkSharp.walkSharpIf(stream, parent, walker);
			case Sharp(ERROR):
				var errorToken:TokenTree = stream.consumeToken();
				parent.addChild(errorToken);
				switch (stream.token()) {
					case Const(CString(_)): errorToken.addChild(stream.consumeToken());
					default:
				}
			case Sharp(ELSEIF):
				WalkSharp.walkSharpElseIf(stream, parent);
			case Sharp(ELSE):
				WalkSharp.walkSharpElse(stream, parent);
			case Sharp(END):
				WalkSharp.walkSharpEnd(stream, parent);
			case Sharp(_):
				parent.addChild(stream.consumeToken());
			default:
		}
	}

	static function walkSharpIf(stream:TokenStream, parent:TokenTree, walker:WalkCB) {
		var ifToken:TokenTree = stream.consumeToken();
		parent.addChild(ifToken);
		WalkSharp.walkSharpIfExpr(stream, ifToken);
		stream.pushSharpIf(ifToken);
		var newParent:TokenTree = ifToken;

		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			try {
				#if cs
				csharpWalkerGuard(stream, newParent, walker);
				#else
				walker(stream, newParent);
				#end
				switch (stream.token()) {
					case BrClose, PClose, Comma:
						var newChild:TokenTree = stream.consumeToken();
						newParent.addChild(newChild);
					default:
				}
			}
			catch (e:SharpElseException) {
				// continue;
				newParent = e.token;
			}
			catch (e:SharpEndException) {
				stream.popSharpIf();
				return;
			}
		}
	}

	#if cs
	static function csharpWalkerGuard(stream:TokenStream, parent:TokenTree, walker:WalkCB) {
		try {
			walker(stream, parent);
		}
		catch (e:SharpElseException) {
			throw e;
		}
		catch (e:SharpEndException) {
			throw e;
		}
		catch (e:Any) {
			var exception:cs.system.Exception = cast(e, cs.system.Exception);
			throw exception.InnerException;
		}
	}
	#end

	static function walkSharpElse(stream:TokenStream, parent:TokenTree) {
		var sharpIfParent:TokenTree = stream.peekSharpIf();
		var ifToken:TokenTree = stream.consumeToken();
		sharpIfParent.addChild(ifToken);
		stream.applyTempStore(sharpIfParent);
		throw new SharpElseException(ifToken);
	}

	static function walkSharpElseIf(stream:TokenStream, parent:TokenTree) {
		var sharpIfParent:TokenTree = stream.peekSharpIf();
		var ifToken:TokenTree = stream.consumeToken();
		sharpIfParent.addChild(ifToken);
		stream.applyTempStore(sharpIfParent);
		WalkSharp.walkSharpIfExpr(stream, ifToken);
		throw new SharpElseException(ifToken);
	}

	static function walkSharpEnd(stream:TokenStream, parent:TokenTree) {
		var sharpIfParent:TokenTree = stream.peekSharpIf();
		var endToken:TokenTree = stream.consumeToken();
		stream.applyTempStore(sharpIfParent);
		sharpIfParent.addChild(endToken);
		throw new SharpEndException();
	}

	static function walkSharpIfExpr(stream:TokenStream, parent:TokenTree) {
		var childToken:TokenTree;
		var progress:TokenStreamProgress = new TokenStreamProgress(stream);
		while (progress.streamHasChanged()) {
			switch (stream.token()) {
				case Unop(OpNot):
					childToken = stream.consumeToken();
					parent.addChild(childToken);
					WalkSharp.walkSharpIfExpr(stream, childToken);
					return;
				case POpen:
					WalkPOpen.walkPOpen(stream, parent);
					return;
				case Kwd(_), Const(CIdent(_)):
					childToken = stream.consumeToken();
					parent.addChild(childToken);
					if (!stream.hasMore()) return;
					switch (stream.token()) {
						case Dot:
						default: return;
					}
					var pos:Null<Position> = stream.getTokenPos();
					if (pos == null) return;
					if (pos.min == childToken.pos.max + 1) continue;

					var dot:TokenTree = stream.consumeToken();
					childToken.addChild(dot);
					WalkSharp.walkSharpIfExpr(stream, dot);
					return;
				default:
					return;
			}
		}
	}
}

typedef WalkCB = TokenStream->TokenTree->Void;

class SharpElseException {
	public var token:TokenTree;

	public function new(token:TokenTree) {
		this.token = token;
	}
}

class SharpEndException {
	public function new() {}
}

enum abstract WalkSharpConsts(String) to String {
	var IF = "if";

	var ELSEIF = "elseif";

	var ELSE = "else";

	var END = "end";

	var ERROR = "error";
}