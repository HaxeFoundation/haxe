package formatter.marker.wrapping;

import formatter.config.WrapConfig;

class MarkWrapping extends MarkWrappingBase {
	public function run() {
		var wrappableTokens:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Dot:
					return FoundGoDeeper;
				case BrOpen:
					return FoundGoDeeper;
				case BkOpen:
					return FoundGoDeeper;
				case POpen:
					return FoundGoDeeper;
				case Binop(OpAdd):
					return FoundGoDeeper;
				case Binop(OpLt):
					return FoundGoDeeper;
				case Binop(OpArrow), Arrow:
					return FoundGoDeeper;
				case CommentLine(_):
					return FoundGoDeeper;
				case Comma:
					wrapAfter(token, true);
					return GoDeeper;
				default:
			}
			return GoDeeper;
		});

		wrappableTokens.reverse();
		for (token in wrappableTokens) {
			switch (token.tok) {
				case Dot:
				case BrOpen:
					markBrWrapping(token);
				case BkOpen:
					arrayWrapping(token);
				case POpen:
					markPWrapping(token);
				case Binop(OpAdd):
					wrapAfter(token, true);
				case Binop(OpLt):
					if (TokenTreeCheckUtils.isTypeParameter(token)) {
						wrapTypeParameter(token);
					}
				case Binop(OpArrow), Arrow:
					wrapAfter(token, true);
				case CommentLine(_):
					wrapBefore(token, false);
				default:
			}
		}

		markMethodChaining(parsedCode.root);
		markMultiVarChaining();
		markImplementsExtendsChaining();
		markOpBoolChaining();
		markOpAddChaining();
		markCasePatternChaining();

		applyWrappingQueue();
	}

	function wrapTypeParameter(token:TokenTree) {
		var close:TokenTree = token.access().firstOf(Binop(OpGt)).token;
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var items:Array<WrappableItem> = makeWrappableItems(token);
		queueWrapping({
			origin: TypeParameterWrapping,
			start: token,
			end: null,
			items: items,
			rules: config.wrapping.typeParameter,
			useTrailing: true,
			overrideAdditionalIndent: null
		}, "wrapTypeParameter");
		return;
	}

	function markBrWrapping(token:TokenTree) {
		switch (TokenTreeCheckUtils.getBrOpenType(token)) {
			case Block:
			case TypedefDecl:
				typedefWrapping(token);
			case ObjectDecl:
				objectLiteralWrapping(token);
			case AnonType:
				anonTypeWrapping(token);
			case Unknown:
		}
	}

	function typedefWrapping(token:TokenTree) {
		var brClose:Null<TokenTree> = getCloseToken(token);
		if (isNewLineBefore(token)) {
			return;
		}
		if (parsedCode.isOriginalSameLine(token, brClose)) {
			noWrap(token, brClose);
			return;
		}
	}

	function anonTypeWrapping(token:TokenTree) {
		var brClose:Null<TokenTree> = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		if (token.index + 1 == brClose.index) {
			whitespace(token, NoneAfter);
			whitespace(brClose, NoneBefore);
			return;
		}
		var next:Null<TokenInfo> = getNextToken(brClose);
		if (next != null) {
			switch (next.token.tok) {
				case BrOpen:
					switch (config.lineEnds.leftCurly) {
						case None, After:
							noLineEndAfter(brClose);
						case Before, Both:
					}
				case Binop(OpGt):
					noLineEndAfter(brClose);
				case Const(CIdent("from")), Const(CIdent("to")):
					noLineEndAfter(brClose);
				case Kwd(_), Const(_):
					lineEndAfter(brClose);
				default:
			}
		}
		if (!parsedCode.isOriginalSameLine(token, brClose)) {
			wrapChildOneLineEach(token, brClose, 0);
			return;
		}

		var items:Array<WrappableItem> = makeWrappableItems(token);

		applyWrappingPlace({
			origin: AnonTypeWrapping,
			start: token,
			end: brClose,
			items: items,
			rules: config.wrapping.anonType,
			useTrailing: true,
			overrideAdditionalIndent: null
		});
	}

	function objectLiteralWrapping(token:TokenTree) {
		var brClose:Null<TokenTree> = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 1)) {
			return;
		}
		if (token.index + 1 == brClose.index) {
			whitespace(token, NoneAfter);
			whitespace(brClose, NoneBefore);
			return;
		}
		if (!parsedCode.isOriginalSameLine(token, brClose)) {
			wrapChildOneLineEach(token, brClose, 0);
			return;
		}
		var minLength:Int = 9999;
		var maxLength:Int = 0;
		var totalLength:Int = 0;
		var itemCount:Int = 0;
		for (child in token.children) {
			switch (child.tok) {
				case BrClose:
					break;
				case CommentLine(_):
					wrapChildOneLineEach(token, brClose, 0);
					return;
				default:
			}
			var length:Int = calcLength(child);
			totalLength += length;
			if (length > maxLength) {
				maxLength = length;
			}
			if (length < minLength) {
				minLength = length;
			}
			itemCount++;
		}
		var lineLength:Int = calcLineLength(token);
		var rule:WrapRule = determineWrapType(config.wrapping.objectLiteral, itemCount, minLength, maxLength, totalLength, lineLength);
		switch (rule.type) {
			case OnePerLine:
				wrapChildOneLineEach(token, brClose, rule.additionalIndent);
			case OnePerLineAfterFirst:
				wrapChildOneLineEach(token, brClose, rule.additionalIndent, true);
			case Keep:
				keep(token, brClose, rule.additionalIndent);
			case EqualNumber:
			case FillLine:
				wrapFillLine(token, brClose, config.wrapping.maxLineLength, rule.additionalIndent);
			case FillLineWithLeadingBreak:
				wrapFillLine(token, brClose, config.wrapping.maxLineLength, rule.additionalIndent);
			case NoWrap:
				noWrap(token, brClose);
				var next:TokenInfo = getNextToken(brClose);
				if (next != null) {
					switch (next.token.tok) {
						case DblDot:
							noLineEndAfter(brClose);
						case Dot, Comma:
							whitespace(brClose, NoneAfter);
						default:
					}
				}
				var prev:TokenInfo = getPreviousToken(token);
				if (prev != null) {
					switch (prev.token.tok) {
						case Kwd(_):
							noLineEndBefore(token);
							whitespace(token, Before);
						case POpen, Binop(_), Comma:
							noLineEndBefore(token);
						default:
					}
				}
		}
	}

	function markPWrapping(token:TokenTree) {
		var pClose:Null<TokenTree> = getCloseToken(token);
		switch (TokenTreeCheckUtils.getPOpenType(token)) {
			case At:
				wrapMetadataCallParameter(token);
			case Parameter:
				wrapFunctionSignature(token);
			case Call:
				wrapCallParameter(token);
			case SwitchCondition:
			case WhileCondition:
			case IfCondition:
			case SharpCondition:
			case Catch:
			case ForLoop:
			case Expression:
		}
	}

	function arrayWrapping(token:TokenTree) {
		switch (TokenTreeCheckUtils.getBkOpenType(token)) {
			case ArrayAccess | ArrayLiteral | Comprehension | Unknown:
				arrayLiteralWrapping(token);
			case MapLiteral:
				mapLiteralWrapping(token);
		}
	}

	function arrayLiteralWrapping(token:TokenTree) {
		var bkClose:Null<TokenTree> = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var items:Array<WrappableItem> = makeWrappableItems(token);
		var itemsWithoutMetadata:Array<WrappableItem> = [];
		for (item in items) {
			switch (item.first.tok) {
				case Kwd(KwdFor), Kwd(KwdWhile):
					if (config.sameLine.comprehensionFor == Keep) {
						return;
					}
					itemsWithoutMetadata.push(item);
				case At:
					if (item.firstLineLength > 30) {
						lineEndBefore(token);
						lineEndBefore(item.first);
					}
				default:
					itemsWithoutMetadata.push(item);
			}
		}
		if (config.wrapping.arrayMatrixWrap != NoMatrixWrap) {
			if (tryMatrixWrap(token, bkClose, itemsWithoutMetadata)) {
				return;
			}
		}
		applyWrappingPlace({
			origin: ArrayWrapping,
			start: token,
			end: bkClose,
			items: itemsWithoutMetadata,
			rules: config.wrapping.arrayWrap,
			useTrailing: true,
			overrideAdditionalIndent: null
		});
	}

	function tryMatrixWrap(open:TokenTree, close:TokenTree, items:Array<WrappableItem>):Bool {
		var prev:Null<WrappableItem> = null;
		var run:Int = 1;
		var lineRun:Int = 0;
		for (index in 0...items.length) {
			var item:WrappableItem = items[index];
			if (prev == null) {
				prev = item;
				continue;
			}
			if (item.multiline) {
				return false;
			}
			if (parsedCode.isOriginalSameLine(prev.first, item.first)) {
				run++;
				prev = item;
				continue;
			}
			if (lineRun != 0) {
				if (lineRun != run) {
					return false;
				}
			}
			lineRun = run;
			run = 1;
			prev = item;
		}
		if (lineRun <= 1) {
			return false;
		}
		if (lineRun != run) {
			return false;
		}
		lineEndAfter(open);

		if (config.wrapping.arrayMatrixWrap == MatrixWrapWithAlign) {
			var maxCols:Array<Int> = [for (i in 0...lineRun) 0];
			for (index in 0...items.length) {
				var item:WrappableItem = items[index];
				var col:Int = index % lineRun;
				if (item.firstLineLength > maxCols[col]) {
					maxCols[col] = item.firstLineLength;
				}
			}

			for (index in 0...items.length) {
				var item:WrappableItem = items[index];
				var expectedLength:Int = maxCols[index % lineRun];
				if (index == items.length - 1) {
					switch (item.last.tok) {
						case Comma:
							expectedLength -= 1;
						default:
							expectedLength -= 2;
					}
				}
				if (item.firstLineLength < expectedLength) {
					spacesBefore(item.first, expectedLength - item.firstLineLength);
				}
			}
		}
		var index:Int = lineRun - 1;
		while (index < items.length) {
			var item:WrappableItem = items[index];
			lineEndAfter(item.last);
			index += lineRun;
		}
		return true;
	}

	function mapLiteralWrapping(token:TokenTree) {
		var bkClose:Null<TokenTree> = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var items:Array<WrappableItem> = makeWrappableItems(token);
		var itemsWithoutMetadata:Array<WrappableItem> = [];
		for (item in items) {
			switch (item.first.tok) {
				case At:
					if (item.firstLineLength > 30) {
						lineEndBefore(token);
						lineEndBefore(item.first);
					}
				default:
					itemsWithoutMetadata.push(item);
			}
		}
		applyWrappingPlace({
			origin: MapWrapping,
			start: token,
			end: bkClose,
			items: itemsWithoutMetadata,
			rules: config.wrapping.mapWrap,
			useTrailing: true,
			overrideAdditionalIndent: null
		});
	}

	override function calcLineLength(token:TokenTree):Int {
		if (token == null) {
			return 0;
		}
		return super.calcLineLength(token);
	}

	function wrapFunctionSignature(token:TokenTree) {
		var pClose:TokenTree = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var rules:WrapRules = config.wrapping.functionSignature;
		switch (token.parent.tok) {
			case Kwd(KwdFunction):
				rules = config.wrapping.anonFunctionSignature;
			default:
		}
		var emptyBody:Bool = hasEmptyFunctionBody(token);
		var items:Array<WrappableItem> = makeWrappableItems(token);
		var addIndent:Null<Int> = null;
		if (emptyBody) {
			addIndent = 0;
		}
		queueWrapping({
			origin: FunctionSignatureWrapping,
			start: token,
			end: pClose,
			items: items,
			rules: rules,
			useTrailing: true,
			overrideAdditionalIndent: addIndent
		}, "wrapFunctionSignature");
	}

	function wrapCallParameter(token:TokenTree) {
		var pClose:TokenTree = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var items:Array<WrappableItem> = makeWrappableItems(token);
		queueWrapping({
			origin: CallParameterWrapping,
			start: token,
			end: pClose,
			items: items,
			rules: config.wrapping.callParameter,
			useTrailing: true,
			overrideAdditionalIndent: null
		}, "wrapCallParameter");
	}

	function wrapMetadataCallParameter(token:TokenTree) {
		var pClose:TokenTree = getCloseToken(token);
		if ((token.children == null) || (token.children.length <= 0)) {
			return;
		}
		var items:Array<WrappableItem> = makeWrappableItems(token);
		queueWrapping({
			origin: MetadataCallParameterWrapping,
			start: token,
			end: pClose,
			items: items,
			rules: config.wrapping.metadataCallParameter,
			useTrailing: false,
			overrideAdditionalIndent: null
		}, "wrapMetadataCallParameter");
	}

	function markMethodChaining(startToken:Null<TokenTree>) {
		if (startToken == null) {
			return;
		}
		var chainStarts:Array<TokenTree> = startToken.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Dot:
					var prev:TokenInfo = getPreviousToken(token);
					while (prev != null) {
						switch (prev.token.tok) {
							case Comment(_):
							case CommentLine(_):
							case PClose:
								wrapBefore(token, true);
								return FoundSkipSubtree;
							default:
								break;
						}
						prev = getPreviousToken(prev.token);
					}
				default:
			}
			return GoDeeper;
		});
		for (chainStart in chainStarts) {
			// look at additional chain starts below
			markInternalMethodChaining(chainStart);
			markSingleMethodChain(chainStart);
		}
	}

	function markInternalMethodChaining(startToken:TokenTree) {
		startToken.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case BkOpen, BrOpen, POpen:
					markMethodChaining(token);
				default:
			}
			return GoDeeper;
		});
	}

	function markSingleMethodChain(chainStart:TokenTree) {
		var chainedCalls:Array<TokenTree> = chainStart.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Dot:
					var prev:TokenInfo = getPreviousToken(token);
					while (prev != null) {
						switch (prev.token.tok) {
							case Comment(_):
							case CommentLine(_):
							case PClose:
								return FoundGoDeeper;
							case Sharp(MarkLineEnds.SHARP_END):
							default:
								break;
						}
						prev = getPreviousToken(prev.token);
					}
					return GoDeeper;
				case POpen, BrOpen, BkOpen:
					return SkipSubtree;
				case Sharp(MarkLineEnds.SHARP_IF):
					if (token.hasChildren()) {
						for (child in token.children) {
							if (child.matches(Dot)) {
								return FoundSkipSubtree;
							}
						}
					}
				default:
			}
			return GoDeeper;
		});

		var firstMethodCall:TokenTree = chainStart.access().parent().isCIdent().parent().matches(Dot).token;
		if (firstMethodCall != null) {
			chainedCalls.unshift(firstMethodCall);
			chainStart = firstMethodCall;
		}

		var items:Array<WrappableItem> = [];
		var chainEnd:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(chainStart);
		var info:TokenInfo = getPreviousToken(chainStart);
		var chainOpen:Null<TokenTree> = chainStart.parent;
		if (info != null) {
			chainOpen = info.token;
		}

		for (index in 0...chainedCalls.length) {
			var child:TokenTree = chainedCalls[index];
			var endToken:TokenTree = chainEnd;
			if (index + 1 < chainedCalls.length) {
				var next:TokenTree = chainedCalls[index + 1];
				info = getPreviousToken(next);
				if (info != null) {
					endToken = info.token;
				}
			}
			items.push(makeWrappableItem(child, endToken));
		}
		chainEnd = null;
		if (chainOpen != null) {
			chainEnd = getCloseToken(chainOpen);
		}
		queueWrapping({
			origin: MethodChainWrapping,
			start: chainOpen,
			end: chainEnd,
			items: items,
			rules: config.wrapping.methodChain,
			useTrailing: false,
			overrideAdditionalIndent: null
		}, "markSingleMethodChain");
	}

	function markOpBoolChaining() {
		var chainStarts:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			if (!token.hasChildren()) {
				return SkipSubtree;
			}
			for (child in token.children) {
				switch (child.tok) {
					case Binop(OpBoolAnd), Binop(OpBoolOr):
						return FoundGoDeeper;
					default:
				}
			}
			return GoDeeper;
		});
		for (chainStart in chainStarts) {
			markSingleOpBoolChain(chainStart);
		}
	}

	function markSingleOpBoolChain(itemStart:TokenTree) {
		var items:Array<WrappableItem> = [];

		var firstItemStart:TokenTree = itemStart;
		switch (itemStart.tok) {
			case Binop(_):
				if (itemStart.previousSibling != null) {
					firstItemStart = itemStart.previousSibling;
				}
			default:
		}
		var prev:Null<TokenInfo> = getPreviousToken(firstItemStart);
		var chainStart:TokenTree = itemStart;
		if (prev != null) {
			chainStart = prev.token;
		}
		var chainEnd:Null<TokenTree> = itemStart.getLastChild();
		if (chainEnd != null) {
			chainEnd = TokenTreeCheckUtils.getLastToken(chainEnd);
			switch (chainEnd.tok) {
				case Semicolon, Comma, PClose:
				default:
					var next:Null<TokenInfo> = getNextToken(chainEnd);
					if (next != null) {
						chainEnd = next.token;
					}
			}
		}
		var first:Bool = true;
		if (itemStart.children != null) {
			for (child in itemStart.children) {
				switch (child.tok) {
					case Binop(OpBoolAnd), Binop(OpBoolOr):
						if (first) {
							itemStart = firstItemStart;
							first = false;
						}
						items.push(makeWrappableItem(itemStart, child));
						var next:Null<TokenInfo> = getNextToken(child);
						if (next == null) {
							return;
						}
						itemStart = next.token;
					default:
						continue;
				}
			}
		}
		items.push(makeWrappableItem(itemStart, TokenTreeCheckUtils.getLastToken(itemStart)));
		queueWrapping({
			origin: OpBoolChainWrapping,
			start: chainStart,
			end: chainEnd,
			items: items,
			rules: config.wrapping.opBoolChain,
			useTrailing: false,
			overrideAdditionalIndent: null
		}, "markSingleOpBoolChain");
	}

	function markCasePatternChaining() {
		var chainStarts:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			return switch (token.tok) {
				case Kwd(KwdCase):
					FoundGoDeeper;
				default:
					GoDeeper;
			}
		});
		for (chainStart in chainStarts) {
			markSingleCasePatternChain(chainStart);
		}
	}

	function markSingleCasePatternChain(itemContainer:TokenTree) {
		var items:Array<WrappableItem> = [];
		// var prev:Null<TokenInfo> = getPreviousToken(findOpAddItemStart(itemContainer));
		var chainStart:TokenTree = itemContainer;
		var chainEnd:Null<TokenTree> = itemContainer.access().firstOf(DblDot).token;
		var next:Null<TokenInfo> = getNextToken(chainStart);
		if (next == null) {
			return;
		}
		var itemStart:TokenTree = next.token;
		if (itemContainer.children != null) {
			for (child in itemContainer.children) {
				switch (child.tok) {
					case DblDot:
						break;
					default:
						var lastToken:TokenTree = TokenTreeCheckUtils.getLastToken(child);
						items.push(makeWrappableItem(child, lastToken));
				}
			}
		}
		queueWrapping({
			origin: CasePatternWrapping,
			start: chainStart,
			end: chainEnd,
			items: items,
			rules: config.wrapping.casePattern,
			useTrailing: false,
			overrideAdditionalIndent: null
		}, "markSingleCasePatternChain");
	}

	function markOpAddChaining() {
		var chainStarts:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			if (!token.hasChildren()) {
				return SkipSubtree;
			}
			for (child in token.children) {
				switch (child.tok) {
					case Binop(OpAdd), Binop(OpSub):
						return FoundGoDeeper;
					default:
				}
			}
			return GoDeeper;
		});
		for (chainStart in chainStarts) {
			markSingleOpAddChain(chainStart);
		}
	}

	function markSingleOpAddChain(itemContainer:TokenTree) {
		var items:Array<WrappableItem> = [];
		var prev:Null<TokenInfo> = getPreviousToken(findOpAddItemStart(itemContainer));
		var chainStart:TokenTree = findOpAddItemStart(itemContainer);
		var chainEnd:Null<TokenTree> = itemContainer.getLastChild();
		switch (chainStart.tok) {
			case POpen:
				var type:POpenType = TokenTreeCheckUtils.getPOpenType(chainStart);
				switch (type) {
					case At:
						return;
					case Parameter:
					case Call:
					case SwitchCondition:
					case WhileCondition:
					case IfCondition:
					case SharpCondition:
					case Catch:
					case ForLoop:
					case Expression:
				}
			default:
		}

		if (chainEnd != null) {
			chainEnd = TokenTreeCheckUtils.getLastToken(chainEnd);
			switch (chainEnd.tok) {
				case Semicolon, Comma:
				default:
					var next:Null<TokenInfo> = getNextToken(chainEnd);
					if (next != null) {
						chainEnd = next.token;
					}
			}
		}
		var next:Null<TokenInfo> = getNextToken(chainStart);
		if (next == null) {
			return;
		}
		var itemStart:TokenTree = next.token;
		if (itemContainer.children != null) {
			for (child in itemContainer.children) {
				switch (child.tok) {
					case Binop(OpAdd), Binop(OpSub):
						items.push(makeWrappableItem(itemStart, child));
						var next:Null<TokenInfo> = getNextToken(child);
						if (next == null) {
							continue;
						}
						itemStart = next.token;
					default:
						continue;
				}
			}
		}
		items.push(makeWrappableItem(itemStart, TokenTreeCheckUtils.getLastToken(itemStart)));
		queueWrapping({
			origin: OpAddChainWrapping,
			start: chainStart,
			end: null,
			items: items,
			rules: config.wrapping.opAddSubChain,
			useTrailing: false,
			overrideAdditionalIndent: null
		}, "markSingleOpAddChain");
	}

	function findOpAddItemStart(itemStart:TokenTree):TokenTree {
		if ((itemStart == null) || (itemStart.tok == Root)) {
			return itemStart;
		}
		var parent:TokenTree = itemStart;
		while ((parent != null) && (parent.tok != Root)) {
			switch (parent.tok) {
				case POpen:
					var pClose:Null<TokenTree> = parent.access().firstOf(PClose).token;
					if ((pClose == null) || (pClose.index > itemStart.index)) {
						return parent;
					}
				case BkOpen:
					var bkClose:Null<TokenTree> = parent.access().firstOf(BkClose).token;
					if ((bkClose == null) || (bkClose.index > itemStart.index)) {
						return parent;
					}
				case BrOpen:
					var brClose:Null<TokenTree> = parent.access().firstOf(BrClose).token;
					if ((brClose == null) || (brClose.index > itemStart.index)) {
						return parent;
					}
				case Binop(OpAssign), Binop(OpAssignOp(_)):
					return parent;
				case Kwd(KwdThis), Kwd(KwdUntyped), Kwd(KwdNull):
				case Kwd(_):
					return parent;
				default:
			}
			itemStart = parent;
			parent = parent.parent;
		}
		return itemStart;
	}

	function makeWrappableItem(start:TokenTree, end:TokenTree):WrappableItem {
		var sameLine:Bool = isSameLineBetween(start, end, false);
		var firstLineLength:Int = 0;
		var lastLineLength:Int = 0;
		if (sameLine) {
			firstLineLength = calcLengthBetween(start, end) + calcTokenLength(end);
		} else {
			firstLineLength = calcLengthUntilNewline(start, end);
			lastLineLength = calcLineLengthBefore(end) + calcTokenLength(end);
		}
		return {
			first: start,
			last: end,
			multiline: !sameLine,
			firstLineLength: firstLineLength,
			lastLineLength: lastLineLength
		}
	}

	function markImplementsExtendsChaining() {
		var classesAndInterfaces:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Kwd(KwdInterface), Kwd(KwdClass):
					return FoundSkipSubtree;
				case Kwd(KwdAbstract), Kwd(KwdEnum), Kwd(KwdTypedef):
					return SkipSubtree;
				default:
					return GoDeeper;
			}
		});
		for (type in classesAndInterfaces) {
			var items:Array<WrappableItem> = [];
			var impls:Array<TokenTree> = type.filterCallback(function(token:TokenTree, index:Int):FilterResult {
				switch (token.tok) {
					case Kwd(KwdExtends), Kwd(KwdImplements):
						return FoundSkipSubtree;
					case Kwd(KwdFunction), Kwd(KwdVar):
						return SkipSubtree;
					default:
						return GoDeeper;
				}
			});
			for (impl in impls) {
				var endToken:TokenTree = TokenTreeCheckUtils.getLastToken(impl);
				items.push(makeWrappableItem(impl, endToken));
			}
			if (items.length <= 0) {
				continue;
			}
			var chainOpen:TokenTree = items[0].first;
			var prev:TokenInfo = getPreviousToken(items[0].first);
			if (prev != null) {
				chainOpen = prev.token;
			}
			var chainEnd:TokenTree = items[items.length - 1].last;
			queueWrapping({
				origin: ImplementsWrapping,
				start: chainOpen,
				end: chainEnd,
				items: items,
				rules: config.wrapping.implementsExtends,
				useTrailing: false,
				overrideAdditionalIndent: null
			}, "markImplementsExtendsChaining");
		}
	}

	function markMultiVarChaining() {
		var allVars:Array<TokenTree> = parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Kwd(KwdVar):
					if ((token.hasChildren()) && (token.children.length > 1)) {
						return FoundSkipSubtree;
					}
					return SkipSubtree;
				default:
					return GoDeeper;
			}
		});
		for (v in allVars) {
			var items:Array<WrappableItem> = [];
			if (v.children == null) {
				continue;
			}
			for (child in v.children) {
				var endToken:TokenTree = TokenTreeCheckUtils.getLastToken(child);
				items.push(makeWrappableItem(child, endToken));
			}
			if (items.length <= 0) {
				continue;
			}
			var chainOpen:TokenTree = v;
			var chainEnd:TokenTree = TokenTreeCheckUtils.getLastToken(v);
			queueWrapping({
				origin: MultiVarWrapping,
				start: chainOpen,
				end: chainEnd,
				items: items,
				rules: config.wrapping.multiVar,
				useTrailing: false,
				overrideAdditionalIndent: null
			}, "markMultiVarChaining");
		}
	}
}
