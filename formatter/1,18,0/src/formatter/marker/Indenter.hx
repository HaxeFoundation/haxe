package formatter.marker;

import formatter.config.IndentationConfig;
#if debugIndent
import sys.io.File;
import sys.io.FileOutput;
#end

class Indenter {
	var config:IndentationConfig;
	var parsedCode:Null<ParsedCode>;
	var indentOffset:Int;

	public function new(config:IndentationConfig) {
		this.config = config;
		indentOffset = 0;
		if (config.character.toLowerCase() == "tab") {
			config.character = "\t";
		}
	}

	public function setParsedCode(parsedCode:ParsedCode) {
		this.parsedCode = parsedCode;
	}

	public function setIndentOffset(indentOffset:Int) {
		if (indentOffset < 0) {
			indentOffset = 0;
		}
		this.indentOffset = indentOffset;
	}

	public function makeIndent(token:TokenTree):String {
		return makeIndentString(calcIndent(token));
	}

	public function makeIndentString(count:Int):String {
		return "".lpad(config.character, config.character.length * count);
	}

	public function calcAbsoluteIndent(indent:Int):Int {
		if (config.character == "\t") {
			return indent * config.tabWidth;
		}
		return indent * config.character.length;
	}

	public function calcIndent(token:TokenTree):Int {
		return calcRealIndent(token) + indentOffset;
	}

	function calcRealIndent(token:TokenTree):Int {
		if (token == null) {
			return 0;
		}
		switch (token.tok) {
			case Sharp(_):
				if (config.conditionalPolicy == FixedZero) {
					return 0;
				}
				if (config.conditionalPolicy == FixedZeroIncrease) {
					return calcConditionalLevel(token);
				}
				if (config.conditionalPolicy == FixedZeroIncreaseBlocks) {
					if (hasBlockParent(token)) {
						return calcConditionalLevel(token);
					}
					return 0;
				}
			default:
		}
		#if debugIndent
		logIndentStart();
		logLine(token);
		#end
		var effectiveToken:TokenTree = findEffectiveParent(token);
		#if debugIndent
		log(effectiveToken, "effectiveParent");
		#end
		return calcFromCandidates(effectiveToken);
	}

	function calcConditionalLevel(token:TokenTree):Int {
		var count:Int = -1;
		while ((token != null) && (token.tok != Root)) {
			switch (token.tok) {
				case Sharp(MarkLineEnds.SHARP_IF):
					count++;
				default:
			}
			token = token.parent;
		}
		if (count <= 0) {
			return 0;
		}
		return count;
	}

	function calcConsecutiveConditionalLevel(token:TokenTree):Int {
		var count:Int = -1;
		var maxCount:Int = -1;

		while ((token != null) && (token.tok != Root)) {
			switch (token.tok) {
				case Sharp(MarkLineEnds.SHARP_IF):
					count++;
				case Sharp(_):
				default:
					if (count > maxCount) {
						maxCount = count;
					}
					count = -1;
			}
			token = token.parent;
		}
		if (count > maxCount) {
			maxCount = count;
		}
		if (maxCount <= 0) {
			return 0;
		}
		return maxCount;
	}

	public function shouldAddTrailingWhitespace():Bool {
		return config.trailingWhitespace;
	}

	function findEffectiveParent(token:TokenTree):TokenTree {
		if (token.tok == Root) {
			return token.getFirstChild();
		}
		switch (token.tok) {
			case BrOpen:
				var parent:TokenTree = token.parent;
				if (parent.tok == Root) {
					return token;
				}
				var firstToken:Null<TokenTree> = findEffectiveParentLineStart(token);
				if (firstToken != null) {
					return firstToken;
				}

				var type:BrOpenType = TokenTreeCheckUtils.getBrOpenType(token);
				switch (type) {
					case Block:
					case TypedefDecl:
						return token.parent;
					case ObjectDecl:
						return token;
					case AnonType:
						return token;
					case Unknown:
				}
				switch (parent.tok) {
					case Kwd(KwdIf), Kwd(KwdElse):
						return findEffectiveParent(parent);
					case Kwd(KwdTry), Kwd(KwdCatch):
						return findEffectiveParent(parent);
					case Kwd(KwdDo), Kwd(KwdWhile), Kwd(KwdFor):
						return findEffectiveParent(parent);
					case Kwd(KwdFunction):
						if (parsedCode.tokenList.isNewLineBefore(parent)) {
							return parent;
						}
						return findEffectiveParent(parent);
					case Kwd(KwdUntyped):
						return findEffectiveParent(parent);
					case Kwd(KwdSwitch):
						return findEffectiveParent(parent);
					case Const(CIdent(_)), Kwd(KwdNew):
						if (parent.parent.tok.match(Kwd(KwdFunction))) {
							return findEffectiveParent(parent.parent);
						}
					case Binop(OpAssign), Binop(OpAssignOp(_)):
						var access:TokenTreeAccessHelper = parent.access().parent().parent().matches(Kwd(KwdTypedef));
						if (access.exists()) {
							return access.token;
						}
					default:
				}
			case POpen:
				var firstToken:Null<TokenTree> = findEffectiveParentLineStart(token);
				if (firstToken != null) {
					return firstToken;
				}
			case BrClose, BkClose:
				return findEffectiveParent(token.parent);
			case Arrow:
				return findEffectiveParent(token.parent);
			case PClose:
				return findEffectiveParent(token.parent);
			case Kwd(KwdIf):
				var prev:Null<TokenInfo> = parsedCode.tokenList.getPreviousToken(token);
				if (prev == null) {
					return token;
				}
				if (prev.whitespaceAfter == Newline) {
					return token;
				}
				var metadata:Null<TokenTree> = token.access().firstOf(At).token;
				if (metadata != null) {
					if (!parsedCode.tokenList.isSameLineBetween(metadata, token, false)) {
						return token;
					}
					prev = parsedCode.tokenList.getPreviousToken(metadata);
					if (prev == null) {
						return token;
					}
					if (prev.whitespaceAfter == Newline) {
						return token;
					}
				}
				var parent:TokenTree = token.parent;
				if (parent.tok == Root) {
					return token;
				}
				switch (parent.tok) {
					case Binop(_):
						return token;
					case Kwd(KwdElse):
					case Kwd(_):
						return token;
					default:
				}
				return findEffectiveParent(token.parent);
			case Kwd(KwdElse), Kwd(KwdCatch):
				return findEffectiveParent(token.parent);
			case Kwd(KwdWhile):
				var parent:TokenTree = token.parent;
				if (parent.tok == Root) {
					return token;
				}
				if ((parent != null) && (parent.tok.match(Kwd(KwdDo)))) {
					return findEffectiveParent(token.parent);
				}
			case Kwd(KwdFunction):
				var parent:TokenTree = token.parent;
				if (parent.tok == Root) {
					return token;
				}
				switch (parent.tok) {
					case POpen:
						if (parsedCode.tokenList.isNewLineBefore(token)) {
							return token;
						}
						return findEffectiveParent(token.parent);
					default:
				}
			case CommentLine(_), Comment(_):
				var next:Null<TokenInfo> = parsedCode.tokenList.getNextToken(token);
				if (next == null) {
					return token;
				}
				switch (next.token.tok) {
					case Kwd(KwdElse):
						return findEffectiveParent(next.token);
					case Kwd(KwdCatch):
						return findEffectiveParent(next.token);
					default:
						return token;
				}
			case Sharp(MarkLineEnds.SHARP_ELSE), Sharp(MarkLineEnds.SHARP_ELSE_IF), Sharp(MarkLineEnds.SHARP_END):
				return findEffectiveParent(token.parent);

			default:
		}
		return token;
	}

	function findEffectiveParentLineStart(token:TokenTree):Null<TokenTree> {
		var firstToken:Null<TokenTree> = parsedCode.tokenList.findLineStartToken(token);
		if (firstToken == null) {
			return null;
		}
		if (firstToken.index == token.index) {
			return null;
		}
		switch (firstToken.tok) {
			case BrClose | BkClose:
				var next:Null<TokenInfo> = parsedCode.tokenList.getNextToken(firstToken);
				if (next != null) {
					switch (next.token.tok) {
						case Comma | Dot | BkClose | BrClose | PClose | Binop(_):
							return findEffectiveParent(firstToken);
						default:
					}
				}
			case PClose:
				var next:Null<TokenInfo> = parsedCode.tokenList.getNextToken(firstToken);
				if (next != null) {
					switch (next.token.tok) {
						case Dot | Binop(_):
							return findEffectiveParent(firstToken);
						default:
					}
				}
			default:
		}

		return null;
	}

	function countLineBreaks(indentingTokensCandidates:Array<TokenTree>, indentComplexValueExpressions:Bool):Int {
		var count:Int = 0;
		var prevToken:Null<TokenTree> = null;
		var currentToken:Null<TokenTree> = null;
		var mustIndent:Bool;
		var lastIndentingToken:Null<TokenTree> = null;
		var skipToToken:TokenTree = null;

		for (token in indentingTokensCandidates) {
			prevToken = currentToken;
			if (prevToken == null) {
				prevToken = token;
			}
			currentToken = token;
			if (skipToToken != null) {
				if (currentToken.index >= skipToToken.index) {
					continue;
				}
				prevToken = skipToToken;
				skipToToken = null;
			}
			if (prevToken.index == currentToken.index) {
				continue;
			}

			switch (currentToken.tok) {
				case BkOpen | BrOpen:
					var close:Null<TokenTree> = parsedCode.tokenList.getCloseToken(currentToken);
					if ((close != null) && (close.index >= 0) && (close.index < prevToken.index)) {
						currentToken = prevToken;
						continue;
					}
				default:
			}

			#if debugIndent
			log(token, '"$prevToken" -> "$currentToken"');
			#end

			mustIndent = false;
			switch (prevToken.tok) {
				case Kwd(KwdIf):
					switch (currentToken.tok) {
						case Binop(OpAssign):
							if (indentComplexValueExpressions) {
								mustIndent = true;
							}
						default:
							if (parsedCode.tokenList.isSameLineBetween(currentToken, prevToken, false)) {
								var elseTok:Null<TokenTree> = prevToken.access().firstOf(Kwd(KwdElse)).token;
								if (elseTok != null) {
									if (parsedCode.tokenList.isSameLineBetween(prevToken, elseTok, false)) {
										continue;
									}
									if (indentComplexValueExpressions) {
										mustIndent = true;
									}
								}
								var brOpen:Null<TokenTree> = prevToken.access().firstOf(BrOpen).token;
								if (brOpen != null) {
									var type:BrOpenType = TokenTreeCheckUtils.getBrOpenType(brOpen);
									switch (type) {
										case Block:
											continue;
										default:
									}
								}
							}
					}
				case Kwd(KwdElse):
					continue;
				case Kwd(KwdCatch):
					if (currentToken.tok.match(Kwd(KwdTry))) {
						continue;
					}
				case Kwd(KwdFunction) | Arrow | BkOpen:
					switch (currentToken.tok) {
						case POpen:
							if (parsedCode.tokenList.isSameLineBetween(currentToken, prevToken, false)) {
								continue;
							}
							if (!parsedCode.tokenList.isNewLineBefore(prevToken)) {
								var firstToken:TokenInfo = parsedCode.tokenList.getPreviousToken(prevToken);
								while (firstToken != null && !parsedCode.tokenList.isNewLineBefore(firstToken.token)) {
									firstToken = parsedCode.tokenList.getPreviousToken(firstToken.token);
								}
								var brOpen:Null<TokenTree> = prevToken.access().firstOf(BrOpen).token;
								if (brOpen != null) {
									if (!parsedCode.tokenList.isSameLineBetween(prevToken, brOpen, false)) {
										continue;
									}
								}
								return count + calcIndent(firstToken.token);
							}
						default:
					}
				case Kwd(KwdSwitch):
					switch (currentToken.tok) {
						case Binop(_):
							if (indentComplexValueExpressions) {
								mustIndent = true;
							}
						case POpen:
							var type:POpenType = TokenTreeCheckUtils.getPOpenType(currentToken);
							switch (type) {
								case At:
								case Parameter:
									mustIndent = true;
								case Call:
								case SwitchCondition:
									mustIndent = true;
								case WhileCondition:
									mustIndent = true;
								case IfCondition:
									mustIndent = true;
								case SharpCondition:
									mustIndent = true;
								case Catch:
									mustIndent = true;
								case ForLoop:
								case Expression:
							}
						default:
					}
				case Kwd(KwdDefault), Kwd(KwdCase):
					if (!config.indentCaseLabels) {
						continue;
					}
				case Dot:
					switch (currentToken.tok) {
						case POpen | BrOpen | BkOpen:
							if (parsedCode.tokenList.isSameLine(currentToken, prevToken)) {
								continue;
							}
							mustIndent = true;
						case Dot:
							if ((prevToken.pos.min == currentToken.pos.min) && parsedCode.tokenList.isNewLineBefore(currentToken)) {
								// first dot & has a newline
							} else {
								continue;
							}
						case Binop(OpAssign) | Binop(OpAssignOp(_)):
							if (parsedCode.tokenList.isSameLineBetween(currentToken, prevToken, false)) {
								continue;
							}
							if (!parsedCode.tokenList.isNewLineBefore(prevToken)) {
								var firstToken:TokenInfo = parsedCode.tokenList.getPreviousToken(prevToken);
								while (firstToken != null && !parsedCode.tokenList.isNewLineBefore(firstToken.token)) {
									firstToken = parsedCode.tokenList.getPreviousToken(firstToken.token);
								}
								return count + calcIndent(firstToken.token);
							}
						case Kwd(KwdReturn), Kwd(KwdUntyped), Kwd(KwdNew):
							if (!parsedCode.tokenList.isNewLineBefore(prevToken)) {
								continue;
							}
						case Kwd(KwdCase) | Kwd(KwdDefault):
							continue;
						default:
							if (parsedCode.tokenList.isNewLineBefore(prevToken)) {
								#if debugIndent
								log(token, 'adds "$prevToken"');
								#end
								count++;
								continue;
							}
					}
				case BrOpen:
					switch (currentToken.tok) {
						case Kwd(KwdAbstract) | Kwd(KwdIf) | Kwd(KwdElse) | Kwd(KwdTry) | Kwd(KwdCatch) | Kwd(KwdDo) | Kwd(KwdWhile) | Kwd(KwdFor) |
							Kwd(KwdFunction) | Kwd(KwdSwitch) | Kwd(KwdReturn) | Kwd(KwdUntyped) | Arrow:
							var type:BrOpenType = TokenTreeCheckUtils.getBrOpenType(prevToken);
							switch (type) {
								case ObjectDecl:
									var brClose:TokenTree = parsedCode.tokenList.getCloseToken(prevToken);
									if ((brClose != null)
										&& (!parsedCode.tokenList.isSameLine(prevToken, brClose))
										&& !config.indentObjectLiteral) {
										continue;
									}
								default:
									continue;
							}
						case POpen, BkOpen:
							if (!parsedCode.tokenList.isNewLineBefore(prevToken)) {
								// continue;
							}
						case Binop(OpAssign) | Binop(OpAssignOp(_)) | DblDot:
							var type:BrOpenType = TokenTreeCheckUtils.getBrOpenType(prevToken);
							switch (type) {
								case ObjectDecl:
									var brClose:TokenTree = parsedCode.tokenList.getCloseToken(prevToken);
									if ((brClose != null)
										&& (!parsedCode.tokenList.isSameLine(prevToken, brClose))
										&& !config.indentObjectLiteral) {
										continue;
									}
								case TypedefDecl:
									continue;
								default:
									// continue;
							}
						default:
					}
				case DblDot:
					switch (currentToken.tok) {
						case Kwd(KwdCase), Kwd(KwdDefault):
							if ((lastIndentingToken != null) && (lastIndentingToken.pos.min == prevToken.pos.min)) {
								continue;
							}
							mustIndent = true;
						default:
					}
				case Const(CIdent("from")), Const(CIdent("to")):
					if (isAbstractFromTo(token) && parsedCode.tokenList.isNewLineBefore(prevToken)) {
						mustIndent = true;
					}
				case Sharp(MarkLineEnds.SHARP_IF):
					if (currentToken.matches(Dot)) {
						if (prevToken.hasChildren()) {
							var isCallChain:Bool = false;
							for (child in prevToken.children) {
								if (child.matches(Dot)) {
									isCallChain = true;
									break;
								}
							}
							if (isCallChain) {
								continue;
							}
						}
					}
				default:
			}
			if (!mustIndent && parsedCode.tokenList.isSameLineBetween(currentToken, prevToken, false)) {
				continue;
			}
			skipToToken = findSkippingToken(currentToken);

			if (!isIndentingToken(currentToken, prevToken)) {
				continue;
			}
			#if debugIndent
			log(token, 'adds "$currentToken"');
			#end
			lastIndentingToken = currentToken;
			count++;
		}
		return count;
	}

	function findSkippingToken(token:TokenTree):Null<TokenTree> {
		var firstToken:Null<TokenTree> = parsedCode.tokenList.findLineStartToken(token);
		if (firstToken == null) {
			return null;
		}
		if (firstToken.index == token.index) {
			return null;
		}
		var skipToToken:Null<TokenTree> = null;
		switch (firstToken.tok) {
			case BkClose | BrClose | PClose:
				skipToToken = findSkippingToken(firstToken.parent);
				if (skipToToken == null) {
					skipToToken = firstToken.parent;
				}
				#if debugIndent
				log(token, 'skipping to "$skipToToken"');
				#end
				return skipToToken;
			default:
				return null;
		}
	}

	function isFieldLevelVar(indentingTokensCandidates:Array<TokenTree>):Bool {
		var tokens:Array<TokenTree> = indentingTokensCandidates.copy();
		tokens.reverse();
		for (token in tokens) {
			switch (token.tok) {
				case Kwd(KwdFunction):
					return false;
				case Kwd(KwdVar):
					return true;
				case Kwd(KwdFinal):
				case Binop(OpAssign):
					return true;
				default:
			}
		}
		return false;
	}

	function calcFromCandidates(token:TokenTree):Int {
		var indentingTokensCandidates:Array<TokenTree> = findIndentingCandidates(token);
		#if debugIndent
		log(token, "candidates: " + [for (candidate in indentingTokensCandidates) '$candidate (${candidate.pos.min})']);
		#end
		if (indentingTokensCandidates.length <= 0) {
			return 0;
		}

		var indentComplexValueExpressions:Bool = config.indentComplexValueExpressions;
		if (isFieldLevelVar(indentingTokensCandidates)) {
			indentComplexValueExpressions = true;
		}
		if (indentComplexValueExpressions) {
			indentingTokensCandidates = compressElseIfCandidates(indentingTokensCandidates);
		}

		var count:Int = countLineBreaks(indentingTokensCandidates, indentComplexValueExpressions);
		if (hasConditional(indentingTokensCandidates)) {
			switch (config.conditionalPolicy) {
				case AlignedDecrease:
					count--;
				case AlignedIncrease:
				case AlignedNestedIncrease:
					count += calcConsecutiveConditionalLevel(token);
				case FixedZero:
				case FixedZeroIncrease:
					count--;
					var conditionalLevel:Int = calcConditionalLevel(token);
					if (conditionalLevel == count) {
						count++;
					}
				case FixedZeroIncreaseBlocks:
					if (hasBlock(indentingTokensCandidates)) {
						count--;
						var conditionalLevel:Int = calcConditionalLevel(token);
						if (conditionalLevel == count) {
							count++;
						}
					}
				case Aligned:
			}
		}
		#if debugIndent
		log(token, "final indent: " + count);
		#end
		return count;
	}

	function hasConditional(tokens:Array<TokenTree>):Bool {
		for (token in tokens) {
			switch (token.tok) {
				case Sharp(MarkLineEnds.SHARP_IF):
					return true;
				default:
			}
		}
		return false;
	}

	function hasBlock(tokens:Array<TokenTree>):Bool {
		for (token in tokens) {
			switch (token.tok) {
				case BrOpen:
					return true;
				default:
			}
		}
		return false;
	}

	function hasBlockParent(token:TokenTree):Bool {
		var parent:TokenTree = token.parent;
		while ((parent != null) && (parent.tok != Root)) {
			switch (parent.tok) {
				case BrOpen:
					return true;
				default:
					parent = parent.parent;
			}
		}
		return false;
	}

	function findIndentingCandidates(token:TokenTree):Array<TokenTree> {
		var indentingTokensCandidates:Array<TokenTree> = [];
		var lastIndentingToken:Null<TokenTree> = null;
		switch (token.tok) {
			case Dot:
				lastIndentingToken = token;
			default:
		}
		indentingTokensCandidates.push(token);
		var parent:Null<TokenTree> = token;
		while ((parent.parent != null) && (parent.parent.tok != Root)) {
			parent = parent.parent;
			if (parent.pos.min > token.pos.min) {
				continue;
			}
			if (isIndentingToken(parent, parent)) {
				if (lastIndentingToken != null) {
					if (lastIndentingToken.tok.match(Dot) && parent.tok.match(Dot)) {
						continue;
					}
				}
				indentingTokensCandidates.push(parent);
				lastIndentingToken = parent;
			} else {
				if (parsedCode.tokenList.isNewLineBefore(parent)) {
					indentingTokensCandidates.push(parent);
					lastIndentingToken = parent;
				}
			}
		}
		return indentingTokensCandidates;
	}

	function compressElseIfCandidates(indentingTokensCandidates:Array<TokenTree>):Array<TokenTree> {
		var compressedCandidates:Array<TokenTree> = [];
		var state:IndentationCompressElseIf = Copy;
		for (token in indentingTokensCandidates) {
			switch (token.tok) {
				case Kwd(KwdIf):
					switch (state) {
						case Copy:
							if (token.access().firstOf(Kwd(KwdElse)).exists()) {
								state = SkipElseIf;
							}
						case SeenElse:
							state = SkipElseIf;
						case SkipElseIf:
							continue;
					}
				case Kwd(KwdElse):
					if ((state == SeenElse) || (state == SkipElseIf)) {
						state = SkipElseIf;
						continue;
					}
					state = SeenElse;
				default:
					state = Copy;
			}
			compressedCandidates.push(token);
		}
		return compressedCandidates;
	}

	function isIndentingToken(token:TokenTree, prevToken:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		switch (token.tok) {
			case BrOpen, BkOpen, POpen, Dot:
				return true;
			case Binop(OpAssign), Binop(OpAssignOp(_)):
				return true;
			case Arrow:
				return true;
			case Binop(OpLt):
				return TokenTreeCheckUtils.isTypeParameter(token);
			case DblDot:
				if ((token.parent.tok.match(Kwd(KwdCase))) || (token.parent.tok.match(Kwd(KwdDefault)))) {
					return true;
				}
				var info:Null<TokenInfo> = parsedCode.tokenList.getTokenAt(token.index);
				if (info == null) {
					return false;
				}
				switch (info.whitespaceAfter) {
					case None, Space:
						return false;
					case Newline:
						return true;
				}
			case Sharp(MarkLineEnds.SHARP_IF):
				switch (config.conditionalPolicy) {
					case AlignedIncrease, AlignedDecrease:
						return true;
					case Aligned:
						return false;
					case AlignedNestedIncrease:
						return false;
					case FixedZero:
						return false;
					case FixedZeroIncrease:
						return true;
					case FixedZeroIncreaseBlocks:
						return (hasBlockParent(token));
				}
			case Kwd(KwdIf), Kwd(KwdElse):
				return true;
			case Kwd(KwdFor), Kwd(KwdDo):
				return true;
			case Kwd(KwdWhile):
				var parent:TokenTree = token.parent;
				if ((parent != null) && (parent.tok.match(Kwd(KwdDo)))) {
					return false;
				}
				return true;
			case Kwd(KwdTry), Kwd(KwdCatch), Kwd(KwdThrow):
				return true;
			case Kwd(KwdFunction):
				return true;
			case Kwd(KwdReturn), Kwd(KwdUntyped):
				return true;
			case Kwd(KwdNew):
				switch (token.parent.tok) {
					case Kwd(KwdFunction):
						return false;
					default:
						return true;
				}
			case Kwd(KwdSwitch), Kwd(KwdCase), Kwd(KwdDefault):
				return true;
			case Kwd(KwdVar):
				return true;
			case Const(CIdent("from")), Const(CIdent("to")):
				return isAbstractFromTo(token);
			case Kwd(KwdAbstract):
				return true;
			case Const(CIdent(_)) if (prevToken.tok.match(Dot)):
				return true;
			default:
		}
		return false;
	}

	function isAbstractFromTo(token:TokenTree):Bool {
		var parent:Null<TokenTree> = token.parent;
		if (parent == null) {
			return false;
		}
		switch (parent.tok) {
			case Const(CIdent(_)):
			default:
				return false;
		}
		parent = parent.parent;
		if (parent == null) {
			return false;
		}
		switch (parent.tok) {
			case Kwd(KwdAbstract):
				return true;
			default:
				return false;
		}
	}

	#if debugIndent
	public static inline var DEBUG_IDENT_LOGFILE:String = "hxformat.log";

	function logIndentStart() {
		var file:FileOutput = File.append(DEBUG_IDENT_LOGFILE, false);
		file.writeString("\n".lpad("-", 202));
		file.close();
	}

	function logLine(token:TokenTree) {
		var pos:LinePos = parsedCode.getLinePos(token.pos.min);
		var text:String = '${pos.line + 1}: ${parsedCode.lines[pos.line]}';
		var file:FileOutput = File.append(DEBUG_IDENT_LOGFILE, false);
		file.writeString(text + "\n");
		file.close();
	}

	function log(token:TokenTree, what:String) {
		var tokenText:String = '`$token` (${token.pos.min})';
		var text:String = '${tokenText.rpad(" ", 50)} ${what.rpad(" ", 90)}';
		var file:FileOutput = File.append(DEBUG_IDENT_LOGFILE, false);
		file.writeString(text + "\n");
		file.close();
	}
	#end
}

enum IndentationCompressElseIf {
	Copy;
	SeenElse;
	SkipElseIf;
}
