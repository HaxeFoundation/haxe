package formatter.marker;

import formatter.config.WhitespaceConfig;
import formatter.config.WhitespacePolicy;

class MarkWhitespace extends MarkerBase {
	public function run() {
		parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Binop(OpLt):
					if (TokenTreeCheckUtils.isTypeParameter(token)) {
						whitespace(token, config.whitespace.typeParamOpenPolicy);
					} else {
						whitespace(token, config.whitespace.binopPolicy);
					}
				case Binop(OpGt):
					markGt(token);
				case Spread | Binop(OpInterval):
					markOpSpread(token);
				case Binop(OpIn) | Kwd(KwdIn):
					markIn(token);
				case Binop(OpMult):
					if (TokenTreeCheckUtils.isImport(token.parent)) {
						whitespace(token, None);
					} else {
						whitespace(token, config.whitespace.binopPolicy);
					}
				case Binop(OpSub):
					if (TokenTreeCheckUtils.filterOpSub(token)) {
						var policy:WhitespacePolicy = config.whitespace.binopPolicy.remove(After);
						var prev:TokenInfo = getPreviousToken(token);
						switch (prev.token.tok) {
							case POpen, BkOpen:
								policy = policy.remove(Before);
							default:
						}
						whitespace(token, policy);
					} else {
						whitespace(token, config.whitespace.binopPolicy);
					}
				case Binop(OpAssign):
					markOpAssign(token);
				case Binop(_):
					whitespace(token, config.whitespace.binopPolicy);
				case Unop(_):
					markUnop(token);
				case Comma:
					whitespace(token, config.whitespace.commaPolicy);
				case Dollar(_):
					markDollar(token);
				case DblDot:
					markDblDot(token);
				case Kwd(_):
					markKeyword(token);
				case Const(CIdent("is")):
					markIs(token);
				case POpen:
					markPOpen(token);
				case PClose:
					markPClose(token);
				case BrOpen:
					markBrOpen(token);
				case BrClose:
					markBrClose(token);
				case BkOpen:
					markBkOpen(token);
				case BkClose:
					markBkClose(token);
				case Question:
					if (TokenTreeCheckUtils.isTernary(token)) {
						whitespace(token, config.whitespace.ternaryPolicy);
					} else {
						whitespace(token, NoneAfter);
					}
				case Sharp(_):
					markSharp(token);
				case Semicolon:
					markSemicolon(token);
				case Const(CIdent("from")), Const(CIdent("to")):
					var parent:Null<TokenTree> = token.access().parent().parent().matches(Kwd(KwdAbstract)).token;
					if (parent != null) {
						whitespace(token, Around);
						wrapBefore(token, true);
					}
					fixConstAfterConst(token);
				case Const(_):
					fixConstAfterConst(token);
				case Arrow:
					markArrow(token);
				case CommentLine(_):
					whitespace(token, Before);
				case Comment(_):
					markComment(token);
				case At:
					markAt(token);
				default:
			}
			return GoDeeper;
		});
	}

	function markOpSpread(token) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		var policy:WhitespacePolicy = config.whitespace.intervalPolicy;
		if (prev != null) {
			policy = switch (prev.token.tok) {
				case Comma:
					config.whitespace.intervalPolicy.add(Before);
				default:
					config.whitespace.intervalPolicy;
			}
		}
		whitespace(token, policy);
	}

	function markGt(token:TokenTree) {
		if (TokenTreeCheckUtils.isOpGtTypedefExtension(token)) {
			whitespace(token, config.whitespace.typeExtensionPolicy);
			return;
		}
		if (TokenTreeCheckUtils.isTypeParameter(token)) {
			var policy:WhitespacePolicy = config.whitespace.typeParamClosePolicy;
			var next:Null<TokenInfo> = getNextToken(token);
			if (next != null) {
				switch (next.token.tok) {
					case Kwd(_):
						policy = policy.add(After);
					case Comma, Semicolon:
						policy = policy.remove(After);
					case Binop(OpGt), PClose, BrClose:
						policy = policy.remove(After);
					default:
				}
			}
			whitespace(token, policy);
		} else {
			whitespace(token, config.whitespace.binopPolicy);
		}
	}

	function fixConstAfterConst(token:TokenTree) {
		var next:Null<TokenInfo> = getNextToken(token);
		if (next != null) {
			switch (next.token.tok) {
				case Const(_), Kwd(_):
					whitespace(token, After);
				default:
			}
		}
	}

	public function successiveParenthesis(token:TokenTree, closing:Bool, policy:WhitespacePolicy, compress:Bool) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev != null) {
			switch (prev.token.tok) {
				case Binop(OpAssign) | Binop(OpAssignOp(_)):
					policy = policy.add(Before);
				case PClose if (token.matches(BkOpen)):
					var type:POpenType = TokenTreeCheckUtils.getPOpenType(prev.token?.parent);
					switch (type) {
						case At | Parameter | SwitchCondition | WhileCondition | IfCondition | SharpCondition | Catch | ForLoop:
							policy = policy.add(Before);
						case Call | Expression:
					}
				case Question | DblDot:
					policy = policy.add(Before);
				default:
			}
		}
		var next:Null<TokenInfo> = getNextToken(token);
		if (next != null) {
			switch (next.token.tok) {
				case Dot | Comma | DblDot | Semicolon | QuestionDot:
					policy = policy.remove(After);
				case Binop(OpGt):
					if (token.tok.match(BrClose)) {
						policy = policy.remove(After);
					}
				case Binop(OpArrow):
					policy = policy.add(After);
				case Kwd(_):
					if (closing) {
						policy = policy.add(After);
					}
				default:
			}
		}
		if (!compress) {
			whitespace(token, policy);
			return;
		}
		if (closing) {
			if (next != null) {
				switch (next.token.tok) {
					case BrClose:
						policy = policy.remove(After);
					case POpen, PClose, BrOpen, BkOpen, BkClose:
						if (token.tok.match(PClose)) {
							switch (TokenTreeCheckUtils.getPOpenType(token.parent)) {
								case Parameter:
									policy = policy.add(After);
								case SwitchCondition | WhileCondition | IfCondition | SharpCondition | Catch:
									policy = policy.add(After);
								case ForLoop:
									policy = policy.add(After);
								case At | Call | Expression:
									policy = policy.remove(After);
							}
						} else {
							policy = policy.remove(After);
						}
					case Binop(OpGt):
						if (TokenTreeCheckUtils.isTypeParameter(next.token)) {
							policy = policy.remove(After);
						}
					default:
				}
			}
		} else {
			if (prev != null) {
				switch (prev.token.tok) {
					case POpen | BrOpen | BkOpen | IntInterval(_) | Spread | Binop(OpInterval):
						policy = policy.remove(Before);
					case PClose:
						switch (TokenTreeCheckUtils.getPOpenType(prev.token)) {
							case SwitchCondition | WhileCondition | IfCondition | SharpCondition | Catch:
								policy = policy.add(Before);
							default:
						}
					case Binop(OpLt):
						if (token.tok.match(BrOpen)) {
							return;
						}
					case DblDot:
						switch (prev.whitespaceAfter) {
							case None:
								policy = policy.remove(Before);
							case Space:
								policy = policy.add(Before);
							case Newline:
						}
					case Arrow:
						return;
					case Comma:
						switch (config.whitespace.commaPolicy) {
							case After, OnlyAfter, Around:
								policy = policy.add(Before);
							default:
						}
					case Const(CIdent("from")) | Const(CIdent("to")):
						var parent:Null<TokenTree> = prev.token.parent;
						if (parent != null) {
							switch (parent.tok) {
								case Const(_):
									policy = policy.add(Before);
								default:
							}
						}
					default:
				}
			}
		}
		whitespace(token, policy);
	}

	function markKeyword(token:TokenTree) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev != null) {
			switch (prev.token.tok) {
				case PClose:
					prev.whitespaceAfter = Space;
				case Const(_):
					prev.whitespaceAfter = Space;
				case At:
					return;
				default:
			}
		}
		switch (token.tok) {
			case Kwd(KwdNull), Kwd(KwdTrue), Kwd(KwdFalse), Kwd(KwdThis), Kwd(KwdDefault), Kwd(KwdContinue):
				var next:Null<TokenInfo> = getNextToken(token);
				if (next != null) {
					switch (next.token.tok) {
						case Kwd(_):
							whitespace(token, After);
							return;
						case Const(CIdent(_)):
							whitespace(token, After);
							return;
						case Question:
							whitespace(token, After);
							return;
						default:
					}
				}
				whitespace(token, NoneAfter);
			case Kwd(KwdExtends), Kwd(KwdImplements):
				whitespace(token, Around);
			case Kwd(KwdIf):
				whitespace(token, config.whitespace.ifPolicy);
			case Kwd(KwdElse):
				var policy:WhitespacePolicy = config.whitespace.ifPolicy;
				whitespace(token, policy.add(After));
			case Kwd(KwdDo):
				whitespace(token, config.whitespace.doPolicy);
			case Kwd(KwdWhile):
				whitespace(token, config.whitespace.whilePolicy);
			case Kwd(KwdFor):
				whitespace(token, config.whitespace.forPolicy);
			case Kwd(KwdSwitch):
				whitespace(token, config.whitespace.switchPolicy);
			case Kwd(KwdTry):
				whitespace(token, config.whitespace.tryPolicy);
			case Kwd(KwdCatch):
				whitespace(token, config.whitespace.catchPolicy);
			case Kwd(KwdReturn):
				whitespace(token, After);
			case Kwd(KwdUntyped):
				whitespace(token, After);
			case Kwd(_):
				var next:Null<TokenInfo> = getNextToken(token);
				if (next != null) {
					switch (next.token.tok) {
						case POpen:
							return;
						case Dot:
							return;
						default:
					}
				}
				whitespace(token, After);
			default:
		}
	}

	function markIs(token:TokenTree) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		var policy:WhitespacePolicy = Around;
		if (prev != null) {
			switch (prev.token.tok) {
				case Dot | POpen:
					policy = policy.remove(Before);
				default:
			}
		}
		whitespace(token, policy);
	}

	function markIn(token:TokenTree) {
		if (!TokenTreeCheckUtils.hasAtParent(token)) {
			whitespace(token, Around);
			return;
		}
		var policy:WhitespacePolicy = After;
		if (token.hasChildren()) {
			policy = None;
		}
		whitespace(token, policy);
	}

	function markOpAssign(token:TokenTree) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev == null) {
			whitespace(token, config.whitespace.binopPolicy);
			return;
		}
		var policy:WhitespacePolicy = config.whitespace.binopPolicy;
		switch (prev.token.tok) {
			case Binop(OpBoolAnd) | Binop(OpBoolOr):
				prev.whitespaceAfter = None;
				policy = policy.remove(Before);
			default:
		}
		whitespace(token, policy);
	}

	function markUnop(token:TokenTree) {
		var next:Null<TokenInfo> = getNextToken(token);
		if (next != null) {
			switch (next.token.tok) {
				case Comma, Semicolon:
					return;
				case PClose, BkClose, BrClose:
					return;
				case Dot:
					switch (token.tok) {
						case Unop(OpNot):
							whitespace(token, None);
							return;
						default:
					}
				default:
			}
		}
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev == null) {
			return;
		}
		switch (prev.token.tok) {
			case Const(CIdent(_)):
				switch (token.tok) {
					case Unop(OpNot), Unop(OpNeg), Unop(OpNegBits):
						return;
					default:
				}
				whitespace(token, After);
			default:
		}
	}

	function markDollar(token:TokenTree) {
		var next:Null<TokenInfo> = getNextToken(token);
		if (next == null) {
			return;
		}
		switch (next.token.tok) {
			case Kwd(_):
				whitespace(token, After);
			case Const(_):
				whitespace(token, After);
			default:
		}
	}

	function markDblDot(token:TokenTree) {
		var type:Null<ColonType> = TokenTreeCheckUtils.getColonType(token);
		if (type == null) {
			type = Unknown;
		}
		var policy:WhitespacePolicy = config.whitespace.colonPolicy;
		switch (type) {
			case SwitchCase:
				policy = config.whitespace.caseColonPolicy;
			case TypeHint:
				policy = config.whitespace.typeHintColonPolicy;
				var parent:TokenTree = token.parent;
				if (parent != null) {
					switch (parent.tok) {
						case Kwd(KwdMacro):
							policy = policy.add(Before);
						default:
					}
				}
			case TypeCheck:
				policy = config.whitespace.typeCheckColonPolicy;
			case Ternary:
				policy = config.whitespace.ternaryPolicy;
			case ObjectLiteral:
				policy = config.whitespace.objectFieldColonPolicy;
			case At:
				whitespace(token, None);
				return;
			case Unknown:
				policy = config.whitespace.colonPolicy;
		}

		policy = correctDblDotSharp(token, policy);
		whitespace(token, policy);
	}

	function correctDblDotSharp(token:TokenTree, policy:WhitespacePolicy):WhitespacePolicy {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev == null) {
			return policy;
		}
		switch (prev.token.tok) {
			case Sharp(MarkLineEnds.SHARP_END):
			case Sharp(_):
				policy = policy.add(Before);
			case PClose, Const(_):
				if (prev.token.parent == null) {
					return policy;
				}
				switch (prev.token.parent.tok) {
					case Sharp(MarkLineEnds.SHARP_IF):
						if (prev.token.parent.getFirstChild().index == prev.token.index) {
							policy = policy.add(Before);
						}
					default:
				}
			default:
		}
		return policy;
	}

	function markSemicolon(token:TokenTree) {
		var next:Null<TokenInfo> = getNextToken(token);
		var policy:WhitespacePolicy = config.whitespace.semicolonPolicy;
		if (next != null) {
			switch (next.token.tok) {
				case BrClose:
					policy = policy.remove(After);
				default:
			}
		}
		whitespace(token, policy);
	}

	function markSharp(token:TokenTree) {
		switch (token.tok) {
			case Sharp(MarkLineEnds.SHARP_IF):
				whitespace(token, After);
				var prev:Null<TokenInfo> = getPreviousToken(token);
				if (prev != null) {
					switch (prev.token.tok) {
						case Const(_) | Kwd(_) | Binop(_):
							whitespace(token, Before);
						default:
					}
				}
				var next:Null<TokenInfo> = getNextToken(token);
				if (next != null) {
					var lastChild:TokenTree = TokenTreeCheckUtils.getLastToken(token.getFirstChild());
					if (lastChild != null) {
						whitespace(lastChild, After);
					}
				}
			case Sharp(MarkLineEnds.SHARP_ELSE_IF):
				whitespace(token, Around);
			case Sharp(MarkLineEnds.SHARP_ELSE):
				whitespace(token, Around);
			case Sharp(MarkLineEnds.SHARP_END):
				var prev:Null<TokenInfo> = getPreviousToken(token);
				if (prev != null) {
					switch (prev.token.tok) {
						case POpen, BrOpen, BkOpen:
						default:
							whitespace(token, Before);
					}
				}
				var next:Null<TokenInfo> = getNextToken(token);
				if (next != null) {
					switch (next.token.tok) {
						case Comma, Semicolon:
						case Const(_), Kwd(_), POpen, BrOpen, BkOpen, Question:
							whitespace(token, After);
						default:
					}
				}
			case Sharp("error"):
				whitespace(token, Around);
			default:
		}
	}

	function markArrow(token:TokenTree) {
		var arrowType:Null<ArrowType> = TokenTreeCheckUtils.getArrowType(token);
		if (arrowType == null) {
			arrowType = ArrowFunction;
		}
		switch (arrowType) {
			case ArrowFunction:
				whitespace(token, config.whitespace.arrowFunctionsPolicy);
			case OldFunctionType:
				whitespace(token, config.whitespace.functionTypeHaxe3Policy);
			case NewFunctionType:
				whitespace(token, config.whitespace.functionTypeHaxe4Policy);
		}
	}

	function determinePOpenPolicy(token:TokenTree):OpenClosePolicy {
		var type:Null<POpenType> = TokenTreeCheckUtils.getPOpenType(token);
		if (type == null) {
			type = Expression;
		}
		switch (type) {
			case At:
				config.whitespace.parenConfig.metadataParens.openingPolicy = config.whitespace.parenConfig.metadataParens.openingPolicy.remove(Before);
				return config.whitespace.parenConfig.metadataParens;
			case Parameter:
				switch (token.parent.tok) {
					case Kwd(KwdReturn):
						var policy:OpenClosePolicy = {
							openingPolicy: config.whitespace.parenConfig.anonFuncParamParens.openingPolicy,
							closingPolicy: config.whitespace.parenConfig.anonFuncParamParens.closingPolicy,
							removeInnerWhenEmpty: config.whitespace.parenConfig.anonFuncParamParens.removeInnerWhenEmpty
						}
						switch (policy.openingPolicy) {
							case None:
								policy.openingPolicy = Before;
							case Before:
							case NoneBefore:
								policy.openingPolicy = Before;
							case OnlyBefore:
							case After:
								policy.openingPolicy = Around;
							case OnlyAfter:
								policy.openingPolicy = Around;
							case NoneAfter:
								policy.openingPolicy = OnlyBefore;
							case Around:
						}
						return policy;
					case Const(CIdent(_)), Kwd(KwdNew):
						return config.whitespace.parenConfig.funcParamParens;
					default:
						return config.whitespace.parenConfig.anonFuncParamParens;
				}
			case Call:
				return config.whitespace.parenConfig.callParens;
			case SwitchCondition:
				if (config.whitespace.parenConfig.switchConditionParens != null) {
					return config.whitespace.parenConfig.switchConditionParens;
				}
				return config.whitespace.parenConfig.conditionParens;
			case WhileCondition:
				if (config.whitespace.parenConfig.whileConditionParens != null) {
					return config.whitespace.parenConfig.whileConditionParens;
				}
				return config.whitespace.parenConfig.conditionParens;
			case IfCondition:
				if (config.whitespace.parenConfig.ifConditionParens != null) {
					return config.whitespace.parenConfig.ifConditionParens;
				}
				return config.whitespace.parenConfig.conditionParens;
			case SharpCondition:
				if (config.whitespace.parenConfig.sharpConditionParens != null) {
					return config.whitespace.parenConfig.sharpConditionParens;
				}
				return config.whitespace.parenConfig.conditionParens;
			case Catch:
				if (config.whitespace.parenConfig.catchParens != null) {
					return config.whitespace.parenConfig.catchParens;
				}
				return config.whitespace.parenConfig.conditionParens;
			case ForLoop:
				return config.whitespace.parenConfig.forLoopParens;
			case Expression:
				return config.whitespace.parenConfig.expressionParens;
		}
		return config.whitespace.parenConfig.expressionParens;
	}

	function markPOpen(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determinePOpenPolicy(token);
		var policy:WhitespacePolicy = openClosePolicy.openingPolicy;
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev != null) {
			switch (prev.token.tok) {
				case Unop(_):
					policy = policy.remove(Before);
				case Binop(_), Comment(_):
					if (prev.spacesAfter > 0) {
						policy = policy.add(Before);
					}
				default:
			}
		}
		if (openClosePolicy.removeInnerWhenEmpty) {
			var next:Null<TokenInfo> = getNextToken(token);
			if (next != null) {
				switch (next.token.tok) {
					case PClose:
						policy = policy.remove(After);
					default:
				}
			}
		}
		successiveParenthesis(token, false, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function markPClose(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determinePOpenPolicy(token.parent);
		var policy:WhitespacePolicy = openClosePolicy.closingPolicy;
		if (openClosePolicy.removeInnerWhenEmpty) {
			var prev:Null<TokenInfo> = getPreviousToken(token);
			if (prev != null) {
				switch (prev.token.tok) {
					case POpen:
						policy = policy.remove(Before);
					default:
				}
			}
		}
		successiveParenthesis(token, true, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function determineBrOpenPolicy(token:TokenTree):OpenClosePolicy {
		var type:Null<BrOpenType> = TokenTreeCheckUtils.getBrOpenType(token);
		if (type == null) {
			type = Unknown;
		}
		switch (type) {
			case Block:
				return config.whitespace.bracesConfig.blockBraces;
			case TypedefDecl:
				return config.whitespace.bracesConfig.typedefBraces;
			case ObjectDecl:
				return config.whitespace.bracesConfig.objectLiteralBraces;
			case AnonType:
				return config.whitespace.bracesConfig.anonTypeBraces;
			case Unknown:
				return config.whitespace.bracesConfig.unknownBraces;
		}
		return config.whitespace.bracesConfig.unknownBraces;
	}

	function markBrOpen(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determineBrOpenPolicy(token);
		var policy:WhitespacePolicy = openClosePolicy.openingPolicy;
		if (openClosePolicy.removeInnerWhenEmpty) {
			var next:Null<TokenInfo> = getNextToken(token);
			if (next != null) {
				switch (next.token.tok) {
					case BrClose:
						policy = policy.remove(After);
					default:
				}
			}
		}
		successiveParenthesis(token, false, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function markBrClose(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determineBrOpenPolicy(token.parent);
		var policy:WhitespacePolicy = openClosePolicy.closingPolicy;
		if (openClosePolicy.removeInnerWhenEmpty) {
			var prev:Null<TokenInfo> = getPreviousToken(token);
			if (prev != null) {
				switch (prev.token.tok) {
					case BrOpen:
						policy = policy.remove(Before);
					default:
				}
			}
		}
		successiveParenthesis(token, true, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function determineBkOpenPolicy(token:TokenTree):OpenClosePolicy {
		var type:Null<BkOpenType> = TokenTreeCheckUtils.getBkOpenType(token);
		if (type == null) {
			type = Unknown;
		}
		switch (type) {
			case ArrayAccess:
				return config.whitespace.bracketConfig.accessBrackets;
			case ArrayLiteral:
				return config.whitespace.bracketConfig.arrayLiteralBrackets;
			case Comprehension:
				return config.whitespace.bracketConfig.comprehensionBrackets;
			case MapLiteral:
				return config.whitespace.bracketConfig.mapLiteralBrackets;
			case Unknown:
				return config.whitespace.bracketConfig.unknownBrackets;
		}
		return config.whitespace.bracketConfig.unknownBrackets;
	}

	function markBkOpen(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determineBkOpenPolicy(token);
		var policy:WhitespacePolicy = openClosePolicy.openingPolicy;
		if (openClosePolicy.removeInnerWhenEmpty) {
			var next:Null<TokenInfo> = getNextToken(token);
			if (next != null) {
				switch (next.token.tok) {
					case BkClose:
						policy = policy.remove(After);
					default:
				}
			}
		}
		successiveParenthesis(token, false, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function markBkClose(token:TokenTree) {
		var openClosePolicy:OpenClosePolicy = determineBkOpenPolicy(token.parent);
		var policy:WhitespacePolicy = openClosePolicy.closingPolicy;
		if (openClosePolicy.removeInnerWhenEmpty) {
			var prev:Null<TokenInfo> = getPreviousToken(token);
			if (prev != null) {
				switch (prev.token.tok) {
					case BkOpen:
						policy = policy.remove(Before);
					default:
				}
			}
		}
		successiveParenthesis(token, true, policy, config.whitespace.compressSuccessiveParenthesis);
	}

	function markComment(token:TokenTree) {
		var policy:WhitespacePolicy = Around;
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev != null) {
			switch (prev.token.tok) {
				case BkOpen, BrOpen, POpen:
					policy = policy.remove(Before);
				case Binop(OpLt):
					if (TokenTreeCheckUtils.isTypeParameter(prev.token)) {
						policy = policy.remove(Before);
					}
				default:
			}
		}
		var next:Null<TokenInfo> = getNextToken(token);
		if (next != null) {
			switch (next.token.tok) {
				case Comma:
					policy = policy.remove(After);
				case BkClose, BrClose, PClose:
					policy = policy.remove(After);
				default:
			}
		}
		whitespace(token, policy);
	}

	function markAt(token:TokenTree) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev == null) {
			return;
		}
		switch (prev.whitespaceAfter) {
			case None:
			case Space, Newline:
				return;
		}
		switch (prev.token.tok) {
			case Root:
				return;
			case Kwd(_):
			case Const(_):
			case Sharp(_):
			case Dollar(_):
			case Unop(_):
			case Binop(OpLt):
				if (TokenTreeCheckUtils.isTypeParameter(prev.token)) {
					return;
				}
			case Binop(_):
			case Comment(_), CommentLine(_):
				return;
			case IntInterval(_):
			case Semicolon:
				return;
			case Dot:
			case DblDot:
			case QuestionDot:
			case Arrow:
			case Comma:
			case BkOpen, BrOpen, POpen:
				return;
			case BkClose:
			case BrClose:
			case PClose:
			case Question:
			case At:
			case Eof:
				return;
			case Spread:
		}
		whitespace(token, Before);
	}
}
