package formatter.marker.wrapping;

import formatter.codedata.ParsedCode;
import formatter.config.Config;
import formatter.config.WrapConfig;
#if debugWrapping
import haxe.PosInfos;
import sys.io.File;
import sys.io.FileOutput;
#end

#if (!macro && !debugWrapping)
@:build(formatter.debug.PosInfosMacro.clean())
#end
class MarkWrappingBase extends MarkerBase {
	var wrappingQueue:Array<WrappingPlace>;

	public function new(config:Config, parsedCode:ParsedCode, indenter:Indenter) {
		super(config, parsedCode, indenter);
		wrappingQueue = [];
	}

	public function noWrap(open:TokenTree, close:TokenTree) {
		var colon:Null<TokenTree> = open.access().matches(BrOpen).parent().matches(DblDot).token;
		if (colon != null) {
			var type:ColonType = TokenTreeCheckUtils.getColonType(colon);
			switch (type) {
				case SwitchCase:
				case TypeHint:
				case TypeCheck:
				case Ternary:
				case ObjectLiteral:
					noLineEndBefore(open);
				case At:
				case Unknown:
			}
		}
		noWrappingBetween(open, close);
		if (open.children != null) {
			for (child in open.children) {
				switch (child.tok) {
					case PClose, BrClose, BkClose:
						break;
					case Binop(OpGt):
						continue;
					case Semicolon, Comma:
						continue;
					default:
				}
				var lastChild:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(child);
				if (lastChild == null) {
					continue;
				} else {
					switch (lastChild.tok) {
						case Comma, Semicolon:
							noLineEndAfter(lastChild);
						default:
					}
				}
			}
		}
		noLineEndBefore(close);
	}

	public function keep2(open:TokenTree, close:Null<TokenTree>, items:Array<WrappableItem>, addIndent:Int, location:WrappingLocation) {
		var tokens:Array<TokenTree> = [];
		// BeforeLast wrapping location
		tokens = [for (item in items) item.last];
		if (items.length > 0) {
			tokens.unshift(items[0].first);
		}
		// AfterLast wrapping location
		tokens = tokens.concat([for (item in items) item.first]);
		if (close != null) {
			tokens.push(close);
		}

		tokens.push(close);
		for (token in tokens) {
			if (parsedCode.isOriginalNewlineBefore(token)) {
				lineEndBefore(token);
				additionalIndent(token, addIndent);
			} else {
				noLineEndBefore(token);
				wrapBefore(token, false);
			}
		}
	}

	public function keep(open:TokenTree, close:TokenTree, addIndent:Int) {
		noWrappingBetween(open, close);

		if (open.children != null) {
			for (child in open.children) {
				var last:Bool = false;
				switch (child.tok) {
					case PClose, BrClose, BkClose:
						last = true;
					case Binop(OpGt):
						continue;
					case Semicolon, Comma:
						continue;
					default:
				}
				if (parsedCode.isOriginalNewlineBefore(child)) {
					lineEndBefore(child);
					additionalIndent(child, addIndent);
				} else {
					noLineEndBefore(child);
					wrapBefore(child, false);
				}
				if (last) {
					break;
				}
			}
		}
		if (!parsedCode.isOriginalNewlineBefore(open)) {
			noLineEndBefore(open);
		}
	}

	public function wrapChildOneLineEach2(open:TokenTree, close:TokenTree, items:Array<WrappableItem>, addIndent:Int = 0, location:WrappingLocation,
			keepFirst:Bool = false) {
		if (items.length <= 0) {
			return;
		}
		switch (location) {
			case BeforeLast:
				var item:WrappableItem = items[0];
				additionalIndent(item.first, addIndent);
				lineEndBefore(item.first);
				item = items.pop();
				for (it in items) {
					additionalIndent(it.last, addIndent);
					lineEndBefore(it.last);
				}
				items.push(item);
			case AfterLast:
				for (item in items) {
					additionalIndent(item.first, addIndent);
					lineEndBefore(item.first);
				}
		}
		if (keepFirst) {
			if (open != null) {
				noLineEndAfter(open);
			}
			var lastToken:TokenTree = items[items.length - 1].last;
			switch (lastToken.tok) {
				case Semicolon:
				default:
					var next:TokenInfo = getNextToken(lastToken);
					if (next == null) {
						noLineEndAfter(lastToken);
						return;
					}
					switch (next.token.tok) {
						case Kwd(KwdThis), Kwd(KwdNull), Kwd(KwdNew):
							noLineEndAfter(lastToken);
						case Kwd(_):
						case Semicolon:
						default:
							noLineEndAfter(lastToken);
					}
			}
		} else {
			var lastToken:TokenTree = items[items.length - 1].last;
			var next:TokenInfo = getNextToken(lastToken);
			if (next == null) {
				lineEndAfter(lastToken);
				return;
			}
			switch (next.token.tok) {
				case Kwd(KwdThis) | Kwd(KwdNull) | Kwd(KwdNew):
					lineEndAfter(lastToken);
				case Kwd(_):
				case BrOpen | POpen | BkOpen:
				case Semicolon:
				case Comma:
				default:
					lineEndAfter(lastToken);
			}
		}
	}

	public function wrapChildOneLineEach(open:TokenTree, close:TokenTree, addIndent:Int = 0, keepFirst:Bool = false) {
		if (!keepFirst) {
			lineEndAfter(open);
		}
		if (open.children == null) {
			return;
		}
		for (child in open.children) {
			switch (child.tok) {
				case PClose, BrClose, BkClose:
					if (keepFirst) {
						noLineEndBefore(child);
					}
					return;
				case Binop(OpGt):
					if (keepFirst) {
						noLineEndBefore(child);
					}
					return;
				case Sharp(_):
					wrapChildOneLineEachSharp(child, addIndent, keepFirst);
				case CommentLine(_):
					var prev:Null<TokenInfo> = getPreviousToken(child);
					if (prev != null) {
						if (parsedCode.isOriginalSameLine(child, prev.token)) {
							noLineEndBefore(child);
						}
					}
					lineEndAfter(child);
					additionalIndent(child, addIndent);
					continue;
				default:
					additionalIndent(child, addIndent);
			}
			var lastChild:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(child);
			if (lastChild == null) {
				lineEndAfter(child);
			} else {
				lineEndAfter(lastChild);
			}
		}
		if (close == null) {
			return;
		}
		switch (close.tok) {
			case BrClose, BkClose, PClose:
				lineEndBefore(close);
			default:
		}
	}

	public function wrapChildOneLineEachSharp(sharp:TokenTree, addIndent:Int = 0, keepFirst:Bool = false) {
		var children:Array<TokenTree> = sharp.children;
		var skipFirst:Bool = false;
		lineEndBefore(sharp);
		switch (sharp.tok) {
			case Sharp(MarkLineEnds.SHARP_IF):
				lineEndAfter(TokenTreeCheckUtils.getLastToken(sharp.getFirstChild()));
				skipFirst = true;
			case Sharp(MarkLineEnds.SHARP_ELSE_IF):
				lineEndAfter(TokenTreeCheckUtils.getLastToken(sharp.getFirstChild()));
				skipFirst = true;
			case Sharp(MarkLineEnds.SHARP_ELSE):
				lineEndAfter(sharp);
			case Sharp(MarkLineEnds.SHARP_END):
				lineEndAfter(sharp);
				return;
			default:
		}
		for (child in children) {
			if (skipFirst) {
				skipFirst = false;
				continue;
			}
			switch (child.tok) {
				case PClose, BrClose, BkClose:
					if (keepFirst) {
						whitespace(child, NoneBefore);
					}
					return;
				case Binop(OpGt):
					if (keepFirst) {
						whitespace(child, NoneBefore);
					}
					return;
				case Sharp(_):
					wrapChildOneLineEachSharp(child, addIndent, keepFirst);
				case CommentLine(_):
					var prev:Null<TokenInfo> = getPreviousToken(child);
					if (prev != null) {
						if (parsedCode.isOriginalSameLine(child, prev.token)) {
							noLineEndBefore(child);
						}
					}
					lineEndAfter(child);
					additionalIndent(child, addIndent);
					continue;
				default:
					additionalIndent(child, addIndent);
			}
		}
	}

	public function wrapFillLine2AfterLast(open:TokenTree, close:TokenTree, items:Array<WrappableItem>, maxLineLength:Int, addIndent:Int = 0,
			useTrailing:Bool = false) {
		if (items.length <= 0) {
			return;
		}
		var lineStart:Null<TokenTree> = open;
		if (lineStart == null) {
			lineStart = items[0].first;
		}
		lineStart = findLineStartToken(lineStart);
		if (lineStart == null) {
			return;
		}

		var indent:Int = indenter.calcIndent(lineStart);
		var lineLength:Int = calcLineLengthBefore(open) + indenter.calcAbsoluteIndent(indent) + calcTokenLength(open);
		var first:Bool = false; // true;
		for (item in items) {
			var tokenLength:Int = item.firstLineLength;
			if (!first && (lineLength + tokenLength >= maxLineLength)) {
				lineEndBefore(item.first);
				additionalIndent(item.first, addIndent);
				lineLength = indenter.calcAbsoluteIndent(indent + 1 + addIndent);
				if (item.multiline) {
					lineLength = item.lastLineLength;
				} else {
					lineLength += item.firstLineLength;
				}
				continue;
			} else {
				noLineEndBefore(item.first);
				lineLength += tokenLength;
				first = false;
				if (item.multiline) {
					lineLength = item.lastLineLength;
				}
			}
		}
		if (useTrailing) {
			var lastItem:WrappableItem = items[items.length - 1];
			var lengthAfter:Int = calcLineLengthAfter(lastItem.last);
			if (lineLength + lengthAfter > maxLineLength) {
				lineEndBefore(lastItem.first);
				additionalIndent(lastItem.first, addIndent);
			}
		}
		noLineEndAfter(open);
		wrapAfter(open, false);
	}

	public function wrapFillLineWithLeading2AfterLast(open:TokenTree, close:TokenTree, items:Array<WrappableItem>, maxLineLength:Int, addIndent:Int = 0) {
		if (items.length <= 0) {
			return;
		}
		var lineStart:Null<TokenTree> = open;
		if (lineStart == null) {
			lineStart = items[0].first;
		}
		lineStart = findLineStartToken(lineStart);
		if (lineStart == null) {
			return;
		}

		var indent:Int = indenter.calcIndent(lineStart);
		var lineLength:Int = indenter.calcAbsoluteIndent(indent + 1 + addIndent);
		var first:Bool = true;
		for (item in items) {
			var tokenLength:Int = item.firstLineLength;
			if (lineLength + tokenLength >= maxLineLength) {
				lineEndBefore(item.first);
				additionalIndent(item.first, addIndent);
				lineLength = indenter.calcAbsoluteIndent(indent + 1 + addIndent);
				if (item.multiline) {
					lineLength = item.lastLineLength;
				} else {
					lineLength += item.firstLineLength;
				}
				continue;
			} else {
				if (!first) {
					noLineEndBefore(item.first);
				} else {
					lineEndBefore(item.first);
				}
				lineLength += tokenLength;
				first = false;
				if (item.multiline) {
					lineLength = item.lastLineLength;
				}
			}
		}
		var lastItem:WrappableItem = items[items.length - 1];
		switch (lastItem.last.tok) {
			case Semicolon:
			case DblDot:
			case BkClose, BrClose, PClose:
				if (isNewLineAfter(lastItem.last)) {
					lineEndAfter(lastItem.last);
				}
			default:
				lineEndAfter(lastItem.last);
		}
	}

	public function wrapFillLine2BeforeLast(open:TokenTree, close:TokenTree, items:Array<WrappableItem>, maxLineLength:Int, addIndent:Int = 0,
			useTrailing:Bool = false) {
		if (items.length <= 0) {
			return;
		}
		var lineStart:Null<TokenTree> = open;
		if (lineStart == null) {
			lineStart = items[0].first;
		}
		lineStart = findLineStartToken(lineStart);
		if (lineStart == null) {
			return;
		}
		var indent:Int = indenter.calcIndent(lineStart);
		var lineLength:Int = calcLineLengthBefore(open) + indenter.calcAbsoluteIndent(indent) + calcTokenLength(open);
		var first:Bool = true;
		for (item in items) {
			var tokenLength:Int = item.firstLineLength;
			if (!first && (lineLength + tokenLength >= maxLineLength)) {
				lineLength = indenter.calcAbsoluteIndent(indent + 1 + addIndent);
				var prev:TokenInfo = getPreviousToken(item.first);
				if (prev != null) {
					lineEndBefore(prev.token);
					additionalIndent(prev.token, addIndent);
					lineLength += prev.text.length;
				}
				if (item.multiline) {
					lineLength += item.lastLineLength;
				} else {
					lineLength += item.firstLineLength;
				}
				continue;
			} else {
				if (first) {
					noLineEndBefore(item.first);
				} else {
					var prev:TokenInfo = getPreviousToken(item.first);
					if (prev != null) {
						noLineEndBefore(prev.token);
					}
				}
				lineLength += tokenLength;
				first = false;
				if (item.multiline) {
					lineLength = indenter.calcAbsoluteIndent(indent + 1 + addIndent) + item.lastLineLength;
				}
			}
		}
		if (useTrailing) {
			var lastItem:WrappableItem = items[items.length - 1];
			var lengthAfter:Int = calcLineLengthAfter(lastItem.last);
			var prev:TokenInfo = getPreviousToken(lastItem.first);
			if ((prev != null) && (lineLength + lengthAfter >= maxLineLength)) {
				lineEndBefore(prev.token);
				additionalIndent(prev.token, addIndent);
			}
		}
		noLineEndAfter(open);
		wrapAfter(open, false);
	}

	public function wrapFillLine(open:TokenTree, close:TokenTree, maxLineLength:Int, addIndent:Int = 0, useTrailing:Bool = false) {
		var lineStart:Null<TokenTree> = findLineStartToken(open);
		if (lineStart == null) {
			return;
		}

		var indent:Int = indenter.calcIndent(lineStart);
		var lineLength:Int = calcLineLengthBefore(open) + indenter.calcAbsoluteIndent(indent + addIndent);
		var first:Bool = true;
		if (open.children == null) {
			return;
		}
		for (child in open.children) {
			switch (child.tok) {
				case PClose, BrClose, BkClose:
					whitespace(child, NoneBefore);
					if (useTrailing) {
						var trailing:Int = calcLineLengthAfter(child);
						if (trailing + lineLength > maxLineLength) {
							var prev:TokenTree = child.previousSibling;
							if (prev == null) {
								return;
							}
							lineEndBefore(prev);
							additionalIndent(prev, addIndent);
						}
					}

					return;
				case Binop(OpGt):
					whitespace(child, NoneBefore);
					return;
				case CommentLine(_):
					var prev:Null<TokenInfo> = getPreviousToken(child);
					if (prev != null) {
						if (parsedCode.isOriginalSameLine(child, prev.token)) {
							noLineEndBefore(child);
						}
					}
					lineEndAfter(child);
					additionalIndent(child, addIndent);
					continue;
				case Kwd(KwdFunction):
					continue;
				case BrOpen:
					continue;
				default:
					additionalIndent(child, addIndent);
			}
			var tokenLength:Int = calcLength(child);
			var lastChild:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(child);
			if (lastChild == null) {
				lastChild = child;
			}
			lineLength += tokenLength;
			if (lineLength > maxLineLength) {
				lineEndBefore(child);
				noLineEndAfter(lastChild);
				indent = indenter.calcIndent(child);
				lineLength = tokenLength + indenter.calcAbsoluteIndent(indent);
			} else {
				noLineEndAfter(lastChild);
			}
			if (first) {
				first = false;
				noLineEndBefore(child);
			}
		}
	}

	override function calcLineLength(token:TokenTree):Int {
		var indent:Int = indenter.calcIndent(token);
		return super.calcLineLength(token) + indenter.calcAbsoluteIndent(indent);
	}

	function hasEmptyFunctionBody(token:TokenTree):Bool {
		var last:Null<TokenTree> = token.getLastChild();
		switch (last.tok) {
			case Semicolon:
				return true;
			default:
		}
		var body:TokenTree = token.nextSibling;
		if (body == null) {
			return true;
		}
		if (body.tok.match(DblDot)) {
			body = body.nextSibling;
		}
		while (body != null && body.tok.match(At)) {
			body = body.nextSibling;
		}
		if (body == null) {
			return true;
		}
		switch (body.tok) {
			case Semicolon:
				return true;
			case BrOpen:
				var brClose:Null<TokenTree> = body.getFirstChild();
				if (brClose == null) {
					return false;
				}
				return brClose.tok.match(BrClose);
			default:
				return false;
		}
	}

	function makeWrappableItems(token:TokenTree):Array<WrappableItem> {
		var items:Array<WrappableItem> = [];
		var lastIndex:Int = -1;
		if (token.children == null) {
			return items;
		}
		for (child in token.children) {
			switch (child.tok) {
				case PClose, BkClose, BrClose:
					return items;
				case Binop(OpGt):
					return items;
				default:
			}
			if (child.index <= lastIndex) {
				continue;
			}
			var endToken:Null<TokenTree> = findItemEnd(child);
			if (endToken == null) {
				continue;
			}
			lastIndex = endToken.index;

			var sameLine:Bool = isSameLineBetween(child, endToken, false);
			var firstLineLength:Int = calcLengthUntilNewline(child, endToken);

			if (isMultilineToken(endToken)) {
				sameLine = false;
			}
			var lastLineLength:Int = 0;
			if (!sameLine) {
				lastLineLength = calcLineLengthAfter(endToken);
			}
			var item:WrappableItem = {
				first: child,
				last: endToken,
				multiline: !sameLine,
				firstLineLength: firstLineLength,
				lastLineLength: lastLineLength
			}
			items.push(item);
		}
		return items;
	}

	function findItemEnd(child:TokenTree):Null<TokenTree> {
		var endToken:Null<TokenTree> = TokenTreeCheckUtils.getLastToken(child);
		if (endToken == null) {
			return null;
		}
		if (endToken.index == child.index) {
			switch (child.tok) {
				case Comment(_) | CommentLine(_):
					var next:Null<TokenTree> = child.nextSibling;
					if (next != null) {
						return findItemEnd(next);
					}
				default:
			}
		}
		switch (endToken.tok) {
			case Comma:
				var next:Null<TokenInfo> = getNextToken(endToken);
				if (next == null) {
					return endToken;
				}
				switch (next.token.tok) {
					case Comment(s), CommentLine(s):
						if (parsedCode.isOriginalSameLine(endToken, next.token)) {
							return next.token;
						}
					default:
				}
				return endToken;
			default:
		}
		var next:Null<TokenInfo> = getNextToken(endToken);
		if (next == null) {
			return endToken;
		}
		switch (next.token.tok) {
			case Binop(OpGt):
				if (next.token.access().parent().matches(Binop(OpLt)).exists()) {
					return endToken;
				}
				return findItemEnd(next.token);
			case Binop(_):
				return findItemEnd(next.token);
			case CommentLine(_), Comment(_):
				return findItemEnd(next.token);
			default:
		}
		return endToken;
	}

	function determineWrapType2(rules:WrapRules, token:TokenTree, items:Array<WrappableItem>, ?pos:PosInfos):WrapRule {
		var itemCount:Int = items.length;
		#if debugWrapping
		logWrappingStart();
		log("itemCount", '$itemCount', pos);
		#end
		if (items.length <= 0) {
			#if debugWrapping
			log("rule", "default", pos);
			#end
			return {
				conditions: [],
				type: rules.defaultWrap,
				location: rules.defaultLocation,
				additionalIndent: rules.defaultAdditionalIndent
			};
		}
		var minItemLength:Int = 9999;
		var maxItemLength:Int = 0;
		var totalItemLength:Int = 0;
		var lineLength:Int = calcLineLength(token);
		var hasMultiLineItem:Bool = false;
		var hasEqualItemLengths:Bool = true;
		var itemLength:Int = -1;
		var count:Int = 0;
		for (item in items) {
			count++;
			totalItemLength += item.firstLineLength + item.lastLineLength;
			if (item.multiline) {
				hasMultiLineItem = true;
			}
			var length:Int = Math.floor(Math.max(item.firstLineLength, item.lastLineLength));
			if (length < minItemLength) {
				minItemLength = length;
			}
			if (length > maxItemLength) {
				maxItemLength = length;
			}
			if (itemLength < 0) {
				itemLength = length;
				continue;
			}
			if (itemLength != length) {
				if (count == items.length && (length + 2) == itemLength) {
					// allow last item to be two characters short e.g. [1,2,3] turns into "1 ,", "2 ,", "3"
					continue;
				}
				hasEqualItemLengths = false;
			}
		}
		#if debugWrapping
		log("maxItemLength", '$minItemLength', pos);
		log("maxItemLength", '$maxItemLength', pos);
		log("totalItemLength", '$totalItemLength', pos);
		log("lineLength", '$lineLength', pos);
		log("hasMultiLineItem", '$hasMultiLineItem', pos);
		log("hasEqualItemLengths", '$hasEqualItemLengths', pos);
		#end
		for (rule in rules.rules) {
			if (matchesRule(rule, itemCount, minItemLength, maxItemLength, totalItemLength, lineLength, hasMultiLineItem, hasEqualItemLengths)) {
				return rule;
			}
		}
		#if debugWrapping
		log("rule", "default", pos);
		#end
		return {
			conditions: [],
			type: rules.defaultWrap,
			location: rules.defaultLocation,
			additionalIndent: rules.defaultAdditionalIndent
		};
	}

	function determineWrapType(rules:WrapRules, itemCount:Int, minItemLength:Int, maxItemLength:Int, totalItemLength:Int, lineLength:Int):WrapRule {
		for (rule in rules.rules) {
			if (matchesRule(rule, itemCount, minItemLength, maxItemLength, totalItemLength, lineLength, false, false)) {
				return rule;
			}
		}
		return {
			conditions: [],
			type: rules.defaultWrap,
			location: rules.defaultLocation,
			additionalIndent: rules.defaultAdditionalIndent
		};
	}

	function matchesRule(rule:WrapRule, itemCount:Int, minItemLength:Int, maxItemLength:Int, totalItemLength:Int, lineLength:Int, hasMultiLineItem:Bool,
			hasEqualItemLenghts:Bool):Bool {
		for (cond in rule.conditions) {
			switch (cond.cond) {
				case ItemCountLargerThan:
					if (itemCount < cond.value) {
						return false;
					}
				case ItemCountLessThan:
					if (itemCount > cond.value) {
						return false;
					}
				case AnyItemLengthLargerThan:
					if (maxItemLength < cond.value) {
						return false;
					}
				case AllItemLengthsLessThan:
					if (maxItemLength > cond.value) {
						return false;
					}
				case AllItemLengthsLargerThan:
					if (minItemLength < cond.value) {
						return false;
					}
				case AnyItemLengthLessThan:
					if (minItemLength > cond.value) {
						return false;
					}
				case TotalItemLengthLargerThan:
					if (totalItemLength < cond.value) {
						return false;
					}
				case TotalItemLengthLessThan:
					if (totalItemLength > cond.value) {
						return false;
					}
				case LineLengthLargerThan:
					if (lineLength < cond.value) {
						return false;
					}
				case LineLengthLessThan:
					if (lineLength > cond.value) {
						return false;
					}
				case HasMultiLineItems:
					if (cond.value == 1) {
						if (!hasMultiLineItem) {
							return false;
						}
					} else {
						if (hasMultiLineItem) {
							return false;
						}
					}
				case ExceedsMaxLineLength:
					if (cond.value == 1) {
						if (lineLength <= config.wrapping.maxLineLength) {
							return false;
						}
					} else {
						if (lineLength > config.wrapping.maxLineLength) {
							return false;
						}
					}
				case EqualItemLengths:
					if (cond.value == 1) {
						if (!hasEqualItemLenghts) {
							return false;
						}
					} else {
						if (hasEqualItemLenghts) {
							return false;
						}
					}
			}
		}
		return true;
	}

	function applyRule(origin:WrappingOrigin, rule:WrapRule, open:TokenTree, close:TokenTree, items:Array<WrappableItem>, addIndent:Int, useTrailing:Bool,
			?pos:PosInfos) {
		var location:WrappingLocation = AfterLast;
		if (rule.location != null) {
			location = rule.location;
		}
		#if debugWrapping
		log("origin", originToText(origin), pos);
		log("rule", '$rule', pos);
		if (open != null) {
			log("open", '`$open` (${open.pos.min})', pos);
		}
		if (close != null) {
			log("close", '`$close` (${close.pos.min})', pos);
		}
		for (item in items) {
			log("item", '$item', pos);
		}
		for (item in items) {
			logWrappableItem(item);
		}
		#end
		switch (rule.type) {
			case OnePerLine:
				wrapChildOneLineEach2(open, close, items, addIndent, location);
			case OnePerLineAfterFirst:
				wrapChildOneLineEach2(open, close, items, addIndent, location, true);
			case Keep:
				keep2(open, close, items, addIndent, location);
			case EqualNumber:
			case FillLine:
				switch (location) {
					case AfterLast:
						wrapFillLine2AfterLast(open, close, items, config.wrapping.maxLineLength, addIndent, useTrailing);
					case BeforeLast:
						wrapFillLine2BeforeLast(open, close, items, config.wrapping.maxLineLength, addIndent, useTrailing);
				}
			case FillLineWithLeadingBreak:
				switch (location) {
					case AfterLast:
						wrapFillLineWithLeading2AfterLast(open, close, items, config.wrapping.maxLineLength, addIndent);
					case BeforeLast:
						wrapFillLine2BeforeLast(open, close, items, config.wrapping.maxLineLength, addIndent, useTrailing);
				}
			case NoWrap:
				noWrappingBetween(open, close, false);
		}
	}

	public function applyWrappingQueue() {
		for (place in wrappingQueue) {
			applyWrappingPlace(place);
		}
	}

	public function applyWrappingPlace(place:WrappingPlace) {
		var rule:WrapRule = determineWrapType2(place.rules, place.start, place.items);
		var additionalIndent:Int = rule.additionalIndent;
		if (place.overrideAdditionalIndent != null) {
			additionalIndent = place.overrideAdditionalIndent;
		}
		applyRule(place.origin, rule, place.start, place.end, place.items, additionalIndent, place.useTrailing);
	}

	function queueWrapping(place:WrappingPlace, name:String) {
		if ((place.items == null) || (place.items.length <= 0)) {
			return;
		}
		var startIndex:Int = getPlaceStartIndex(place);
		var endIndex:Int = getPlaceEndIndex(place);
		if ((startIndex < 0) || (endIndex < 0)) {
			return;
		}
		var index:Int = 0;
		for (index in 0...wrappingQueue.length) {
			var p:WrappingPlace = wrappingQueue[index];
			var itemStart:Int = getPlaceStartIndex(p);
			if (startIndex > itemStart) {
				continue;
			}
			if (startIndex == itemStart) {
				var itemEnd:Int = getPlaceEndIndex(p);
				if (endIndex > itemEnd) {
					wrappingQueue.insert(index, place);
					return;
				}
				continue;
			}
			wrappingQueue.insert(index, place);
			return;
		}
		wrappingQueue.push(place);
	}

	function getPlaceStartIndex(place:WrappingPlace):Int {
		if ((place.items == null) || (place.items.length <= 0)) {
			return -1;
		}
		if (place.start != null) {
			return place.start.index;
		} else {
			return place.items[0].first.index;
		}
	}

	function getPlaceEndIndex(place:WrappingPlace):Int {
		if ((place.items == null) || (place.items.length <= 0)) {
			return -1;
		}
		if (place.end != null) {
			return place.end.index;
		} else {
			return place.items[place.items.length - 1].last.index;
		}
	}

	#if debugWrapping
	function logWrappingStart() {
		#if !js
		var file:FileOutput = File.append("hxformat.log", false);
		file.writeString("\n".lpad("-", 202));
		file.close();
		#end
	}

	function log(what:String, value:String, ?pos:PosInfos) {
		#if !js
		var func:String = '${pos.fileName}:${pos.lineNumber}:${pos.methodName}';
		var text:String = '${func.rpad(" ", 90)} ${what.rpad(" ", 20)} ${value.rpad(" ", 90)}';
		var file:FileOutput = File.append("hxformat.log", false);
		file.writeString(text + "\n");
		file.close();
		#end
	}

	function logWrappableItem(item:WrappableItem, ?pos:PosInfos) {
		if (item.last == null) {
			return;
		}
		var text:String = "";
		for (index in item.first.index...item.last.index + 1) {
			if (text != "") {
				text += " ";
			}
			text += '${parsedCode.tokens[index]}';
		}
		log("code", text, pos);
	}

	function originToText(origin:WrappingOrigin):String {
		return switch (origin) {
			case AnonTypeWrapping:
				"AnonTypeWrapping";
			case ArrayWrapping:
				"ArrayWrapping";
			case CallParameterWrapping:
				"CallParameterWrapping";
			case CasePatternWrapping:
				"CasePatternWrapping";
			case FunctionSignatureWrapping:
				"FunctionSignatureWrapping";
			case ImplementsWrapping:
				"ImplementsWrapping";
			case MapWrapping:
				"MapWrapping";
			case MetadataCallParameterWrapping:
				"MetadataCallParameterWrapping";
			case MethodChainWrapping:
				"MethodChainWrapping";
			case MultiVarWrapping:
				"MultiVarWrapping";
			case OpAddChainWrapping:
				"OpAddChainWrapping";
			case OpBoolChainWrapping:
				"OpBoolChainWrapping";
			case TypeParameterWrapping:
				"TypeParameterWrapping";
		}
	}
	#end
}

typedef WrappingPlace = {
	var origin:WrappingOrigin;
	var start:TokenTree;
	var end:Null<TokenTree>;
	var items:Array<WrappableItem>;
	var rules:WrapRules;
	var useTrailing:Bool;
	var overrideAdditionalIndent:Null<Int>;
}

enum WrappingOrigin {
	AnonTypeWrapping;
	ArrayWrapping;
	MapWrapping;
	CallParameterWrapping;
	CasePatternWrapping;
	FunctionSignatureWrapping;
	ImplementsWrapping;
	MetadataCallParameterWrapping;
	MethodChainWrapping;
	MultiVarWrapping;
	OpAddChainWrapping;
	OpBoolChainWrapping;
	TypeParameterWrapping;
}
