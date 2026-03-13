package tokentree.walk;

import tokentree.utils.TokenTreeCheckUtils;

class WalkStatement {
	public static function walkStatement(stream:TokenStream, parent:TokenTree) {
		walkStatementWithoutSemicolon(stream, parent);
		if (stream.tokenForMatch().match(Semicolon)) {
			var semicolon:TokenTree = stream.consumeToken();

			var lastChild:Null<TokenTree> = switch (parent.tok) {
				case Binop(OpAdd) | Binop(OpSub) | Binop(OpBoolAnd) | Binop(OpBoolOr): parent.parent.getLastChild();
				default: parent.getLastChild();
			}

			if (lastChild == null) {
				lastChild = parent;
			}
			switch (lastChild.tok) {
				case BrClose, BkClose, PClose:
					lastChild = parent;
				default:
			}
			lastChild.addChild(semicolon);
		}
	}

	static function walkStatementWithoutSemicolon(stream:TokenStream, parent:TokenTree) {
		WalkComment.walkComment(stream, parent);

		var wantMore:Bool = true;

		WalkAt.walkAts(stream);
		switch (stream.token()) {
			case Binop(OpSub):
				WalkBinopSub.walkBinopSub(stream, parent);
				return;
			case Binop(OpLt):
				if (stream.isTypedParam()) {
					WalkLtGt.walkLtGt(stream, parent);
					if (stream.tokenForMatch().match(Arrow)) {
						walkStatementWithoutSemicolon(stream, parent);
					}
					if (stream.tokenForMatch().match(POpen)) {
						walkStatementWithoutSemicolon(stream, parent);
					}
					return;
				}
				wantMore = true;
			case Binop(OpGt):
				var gtTok:TokenTree = stream.consumeOpGt();
				parent.addChild(gtTok);
				walkStatementWithoutSemicolon(stream, gtTok);
				return;
			case Binop(OpOr):
				if ((parent.parent != null) && (parent.parent.tok != Root)) {
					switch (parent.parent.tok) {
						case Kwd(KwdCase):
							var orTok:TokenTree = stream.consumeToken();
							parent.addChild(orTok);
							walkStatementWithoutSemicolon(stream, parent.parent);
							return;
						default:
					}
				}
				wantMore = true;
			case Binop(_):
				wantMore = true;
			case Const(CIdent("is")):
				wantMore = true;
			case Unop(_):
				if (parent.isCIdentOrCString()) {
					var newChild:TokenTree = stream.consumeToken();
					parent.addChild(newChild);
					if (!stream.hasMore()) return;
					switch (stream.token()) {
						case Dot | Binop(_) | Const(CIdent("is")): walkStatementWithoutSemicolon(stream, newChild);
						default:
					}
					return;
				}
			case IntInterval(_) | Spread:
				wantMore = true;
			case Const(CIdent("final")) | Kwd(_):
				if (walkKeyword(stream, parent)) wantMore = true;
				else return;
			case Arrow:
				wantMore = true;
			case BrOpen:
				WalkBlock.walkBlock(stream, parent);
				return;
			case BkOpen:
				WalkArrayAccess.walkArrayAccess(stream, parent);
				walkStatementContinue(stream, parent);
				return;
			case Dollar(name):
				walkDollarStatement(stream, parent);
				return;
			case POpen:
				walkPOpen(stream, parent);
				return;
			case Question:
				WalkQuestion.walkQuestion(stream, parent);
				return;
			case PClose, BrClose, BkClose:
				return;
			case Comma:
				return;
			case Semicolon:
				return;
			case Sharp(_):
				WalkSharp.walkSharp(stream, parent, walkStatement);
				walkStatementContinueAfterSharp(stream, parent);
				return;
			case Dot | QuestionDot:
				wantMore = true;
			case DblDot:
				switch (parent.tok) {
					case Dot: return;
					case Kwd(KwdMacro):
						walkDblDot(stream, parent);
						return;
					default:
				}
				if (parent.tok.match(Dot)) {
					return;
				}
				if (WalkQuestion.isTernary(stream, parent)) {
					walkStatementContinue(stream, parent);
					return;
				}
				wantMore = true;
			default:
				wantMore = false;
		}
		var newChild:TokenTree = stream.consumeToken();
		parent.addChild(newChild);
		stream.applyTempStore(newChild);
		walkTrailingComment(stream, newChild);

		if (wantMore) walkStatementWithoutSemicolon(stream, newChild);
		walkStatementContinue(stream, newChild);
		walkTrailingComment(stream, newChild);
	}

	public static function walkTrailingComment(stream:TokenStream, parent:TokenTree) {
		if (!stream.hasMore()) {
			return;
		}
		switch (stream.token()) {
			case CommentLine(_):
				var currentPos:Int = stream.getStreamIndex();
				var commentTok:TokenTree = stream.consumeToken();
				if (!stream.tokenForMatch().match(Kwd(KwdElse))) {
					stream.rewindTo(currentPos);
					return;
				}
				parent.addChild(commentTok);
			default:
		}
	}

	public static function walkStatementContinue(stream:TokenStream, parent:TokenTree) {
		if (!stream.hasMore()) return;
		switch (stream.token()) {
			case Dot | QuestionDot:
				walkStatementWithoutSemicolon(stream, parent);
			case DblDot:
				walkDblDot(stream, parent);
			case Semicolon:
				return;
			case Arrow:
				walkStatementWithoutSemicolon(stream, parent);
			case Binop(OpBoolAnd), Binop(OpBoolOr):
				walkOpBool(stream, parent);
			case Binop(OpAdd), Binop(OpSub):
				walkOpAdd(stream, parent);
			case Binop(OpGt):
				var ltParent:TokenTree = parent;
				while (true) {
					switch (ltParent.tok) {
						case Root: break;
						case Dot | DblDot | Comma | Arrow | POpen | Const(_) | Dollar(_) | BkOpen | BrOpen | Binop(OpGt): ltParent = ltParent.parent;
						case Binop(OpLt): return;
						default: break;
					}
				}
				walkStatementWithoutSemicolon(stream, parent);
			case Binop(_):
				walkStatementWithoutSemicolon(stream, parent);
			case Const(CIdent("is")):
				walkStatementWithoutSemicolon(stream, parent);
			case Unop(_):
				if (parent.isCIdentOrCString()) {
					walkStatementWithoutSemicolon(stream, parent);
				}
			case Question:
				WalkQuestion.walkQuestion(stream, parent);
			case BkOpen:
				walkStatementWithoutSemicolon(stream, parent);
			case POpen:
				walkStatementWithoutSemicolon(stream, parent);
			case CommentLine(_), Comment(_):
				var nextTokDef:Null<TokenTreeDef> = stream.peekNonCommentToken();
				if (nextTokDef == null) {
					return;
				}
				switch (nextTokDef) {
					case Dot, DblDot, Binop(_), Unop(_), Question:
						WalkComment.walkComment(stream, parent);
						walkStatementContinue(stream, parent);
					default:
				}
			case Spread:
				walkStatementWithoutSemicolon(stream, parent);
			default:
		}
	}

	static function walkKeyword(stream:TokenStream, parent:TokenTree):Bool {
		switch (stream.token()) {
			case Kwd(KwdVar):
				WalkVar.walkVar(stream, parent);
			case Kwd(KwdFinal):
				WalkFinal.walkFinal(stream, parent);
			case Kwd(KwdInline):
				return walkInline(stream, parent);
			case Kwd(KwdStatic) | Kwd(KwdPublic) | Kwd(KwdPrivate):
				stream.addToTempStore(stream.consumeToken());
				return false;
			case Kwd(KwdNew):
				if (parent.tok.match(Dot)) {
					var newChild:TokenTree = stream.consumeToken();
					parent.addChild(newChild);
					walkStatementContinue(stream, newChild);
				}
				else {
					WalkNew.walkNew(stream, parent);
				}
			case Kwd(KwdFor):
				WalkFor.walkFor(stream, parent);
			case Kwd(KwdFunction):
				WalkFunction.walkFunction(stream, parent);
			case Kwd(KwdClass):
				WalkClass.walkClass(stream, parent);
			case Kwd(KwdMacro), Kwd(KwdReturn):
				return true;
			case Kwd(KwdSwitch):
				WalkSwitch.walkSwitch(stream, parent);
			case Kwd(KwdCase):
				return false;
			case Kwd(KwdDefault):
				// switch or property
				if (parent.tok.match(BrOpen)) return false;
				return true;
			case Kwd(KwdIf):
				WalkIf.walkIf(stream, parent);
			case Kwd(KwdTry):
				WalkTry.walkTry(stream, parent);
			case Kwd(KwdDo):
				WalkDoWhile.walkDoWhile(stream, parent);
			case Kwd(KwdWhile):
				if (!parent.tok.match(BrOpen) && parent.parent.tok.match(Kwd(KwdDo))) {
					return false;
				}
				WalkWhile.walkWhile(stream, parent);
			case Kwd(KwdNull), Kwd(KwdTrue), Kwd(KwdFalse):
				var newChild:TokenTree = stream.consumeToken();
				parent.addChild(newChild);
				if (!stream.hasMore()) {
					return false;
				}
				switch (stream.token()) {
					case Binop(OpBoolAnd), Binop(OpBoolOr): walkOpBool(stream, newChild);
					case Question: WalkQuestion.walkQuestion(stream, newChild);
					case Binop(_): walkStatementWithoutSemicolon(stream, newChild);
					default:
				}
				return false;
			case Kwd(KwdCast):
				var newChild:TokenTree = stream.consumeToken();
				parent.addChild(newChild);
				walkStatementWithoutSemicolon(stream, newChild);
				return false;
			case Kwd(KwdThis):
				var newChild:TokenTree = stream.consumeToken();
				parent.addChild(newChild);
				walkStatementContinue(stream, newChild);
				return false;
			default:
				return true;
		}
		return false;
	}

	static function walkInline(stream:TokenStream, parent:TokenTree):Bool {
		stream.addToTempStore(stream.consumeToken());
		return switch (stream.token()) {
			case Const(_) | Kwd(KwdNew): true;
			case Kwd(KwdFunction):
				WalkFunction.walkFunction(stream, parent);
				false;
			default: false;
		}
	}

	public static function walkDblDot(stream:TokenStream, parent:TokenTree) {
		var question:Null<TokenTree> = findQuestionParent(stream, parent);
		if (question != null) {
			return;
		}
		var dblDotTok:TokenTree = stream.consumeToken();
		parent.addChild(dblDotTok);
		if (parent.isCIdentOrCString() && parent.parent.tok.match(BrOpen)) {
			walkStatementWithoutSemicolon(stream, dblDotTok);
			return;
		}
		if (stream.tokenForMatch().match(Kwd(KwdNew))) {
			WalkNew.walkNew(stream, dblDotTok);
			return;
		}
		if (!walkKeyword(stream, dblDotTok)) return;
		WalkTypeNameDef.walkTypeNameDef(stream, dblDotTok);
		if (stream.tokenForMatch().match(Binop(OpAssign))) {
			walkStatementWithoutSemicolon(stream, parent);
		}
		if (stream.tokenForMatch().match(Arrow)) {
			walkStatementWithoutSemicolon(stream, parent);
		}
	}

	static function walkPOpen(stream:TokenStream, parent:TokenTree) {
		var pOpen:TokenTree = WalkPOpen.walkPOpen(stream, parent);
		if (parent.tok == Root) {
			return;
		}
		if (parent.isCIdent()) {
			walkStatementContinue(stream, parent);
		}
		else {
			switch (parent.tok) {
				case Kwd(KwdIf) | Kwd(KwdSwitch) | Kwd(KwdFor) | Kwd(KwdWhile):
					switch (stream.token()) {
						case Binop(OpSub): return;
						case Binop(_):
						case Dot:
						default: return;
					}
				default:
			}
			walkStatementContinue(stream, pOpen);
		}
	}

	static function findQuestionParent(stream:TokenStream, token:TokenTree):Null<TokenTree> {
		var parent:Null<TokenTree> = token;
		while (parent != null && parent.tok != Root) {
			switch (parent.tok) {
				case Question:
					if (WalkQuestion.isTernary(stream, parent)) return parent;
					return null;
				case Comma:
					return null;
				case BrOpen:
					if (!TokenTreeAccessHelper.access(parent).firstOf(BrClose).exists()) {
						return null;
					}
				case POpen:
					if (!TokenTreeAccessHelper.access(parent).firstOf(PClose).exists()) {
						return null;
					}
				case Kwd(KwdReturn):
					return parent;
				case Kwd(KwdCase):
					return parent;
				case Kwd(KwdMacro):
					if (parent.index + 1 == stream.getStreamIndex()) return null;
					parent = findQuestionParent(stream, parent.parent);
					if (parent == null) {
						return null;
					}
					switch (parent.tok) {
						case Kwd(KwdCase) | Kwd(KwdDefault) | Question: return parent;
						default: return null;
					}
					return null;
				case Kwd(KwdDefault):
					return parent;
				case Binop(_):
					return parent;
				case DblDot:
					var type:ColonType = TokenTreeCheckUtils.determineColonType(parent);
					switch (type) {
						case SwitchCase | At: return null;
						case TypeHint | TypeCheck | Ternary | ObjectLiteral | Unknown:
					}
				default:
			}
			parent = parent.parent;
		}
		return null;
	}

	static function walkStatementContinueAfterSharp(stream:TokenStream, parent:TokenTree) {
		switch (stream.token()) {
			case Kwd(KwdCase), Kwd(KwdDefault):
				var lastChild:Null<TokenTree> = parent.getLastChild();
				if (lastChild == null) {
					lastChild = parent;
				}
				WalkSwitch.walkSwitchCases(stream, lastChild);
			default:
		}
	}

	static function walkOpBool(stream:TokenStream, token:TokenTree) {
		var parent = token.parent;
		while (parent.tok != Root) {
			switch (parent.tok) {
				case Binop(OpAssign), Binop(OpAssignOp(_)):
					break;
				case Binop(OpBoolAnd), Binop(OpBoolOr):
					token = parent.parent;
					break;
				case POpen:
					if (token.tok.match(POpen)) {
						token = parent;
					}
					break;
				case Kwd(KwdReturn), Kwd(KwdUntyped), Kwd(KwdIf), Kwd(KwdWhile), Kwd(KwdThrow):
					break;
				case Kwd(KwdFunction), Arrow, Question:
					break;
				case Kwd(KwdSwitch), Kwd(KwdCase), Kwd(KwdDefault):
					break;
				case DblDot:
					token = parent;
					break;
				default:
					token = parent;
					parent = parent.parent;
			}
		}
		walkStatementWithoutSemicolon(stream, token);
	}

	static function walkOpAdd(stream:TokenStream, token:TokenTree) {
		var parent = token.parent;
		while (parent.tok != Root) {
			switch (parent.tok) {
				case Binop(OpAssign), Binop(OpAssignOp(_)):
					break;
				case IntInterval(_), BkOpen, BrOpen:
					break;
				case Binop(OpMult), Binop(OpDiv):
					token = parent;
					parent = parent.parent;
				case Binop(OpAdd), Binop(OpSub):
					token = parent.parent;
					break;
				case Binop(_):
					break;
				case POpen:
					var pClose:Null<TokenTree> = parent.access().firstOf(PClose).token;
					if (pClose == null) {
						token = parent;
						break;
					}
					token = parent;
					parent = parent.parent;
				case Kwd(KwdReturn), Kwd(KwdUntyped), Kwd(KwdIf), Kwd(KwdWhile), Kwd(KwdThrow):
					break;
				case Kwd(KwdFunction), Arrow, Question:
					break;
				case DblDot:
					break;
				default:
					token = parent;
					parent = parent.parent;
			}
		}
		walkStatementWithoutSemicolon(stream, token);
	}

	static function walkDollarStatement(stream:TokenStream, parent:TokenTree) {
		var dollarTok:TokenTree = stream.consumeToken();
		parent.addChild(dollarTok);
		switch (stream.token()) {
			case POpen | BrOpen | BkOpen | Dot | Binop(_) | Const(CIdent("is")):
				WalkBlock.walkBlock(stream, dollarTok);
			default:
		}
	}
}