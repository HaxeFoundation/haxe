package formatter.codedata;

import formatter.config.Config;
import formatter.config.WhitespacePolicy;
import formatter.marker.Indenter;
import formatter.marker.MarkLineEnds;
#if debugLog
import haxe.PosInfos;
import sys.io.File;
import sys.io.FileOutput;
#end

#if (!macro && !debugLog)
@:build(formatter.debug.PosInfosMacro.clean())
#end
class TokenList {
	static inline var BAD_OFFSET:String = "Bad offset";
	static inline var NEWLINE_TO_SPACE:String = "Newline -> Space";

	public var tokens:Array<Null<TokenInfo>>;
	public var leadingEmptyLInes:Int;

	var closeTokenCache:Map<Int, TokenTree>;

	public function new() {
		tokens = [];
		leadingEmptyLInes = 0;
		closeTokenCache = new Map<Int, TokenTree>();
		#if debugLog
		logFileStart();
		#end
	}

	public function buildList(token:TokenTree) {
		if (token.children == null) {
			return;
		}

		for (child in token.children) {
			var index:Int = child.index;
			if ((index < 0) || (child.inserted)) {
				continue;
			}
			if (child.index >= tokens.length) {
				fill(child.index - tokens.length);
			}
			tokens[index] = {
				token: child,
				whitespaceAfter: None,
				spacesBefore: 0,
				spacesAfter: 0,
				emptyLinesAfter: 0,
				wrapAfter: false,
				text: null,
				additionalIndent: 0
			};
			buildList(child);
		}
	}

	function fill(count:Int) {
		while (count-- > 0) {
			tokens.push(null);
		}
	}

	public function getCloseToken(token:TokenTree):Null<TokenTree> {
		if ((token == null) || (token.tok == Root)) {
			return null;
		}
		if ((token.index < 0) || (token.index >= tokens.length)) {
			return null;
		}
		if (closeTokenCache.exists(token.index)) {
			return closeTokenCache.get(token.index);
		}
		var result:TokenTree;
		switch (token.tok) {
			case POpen:
				result = token.access().firstOf(PClose).token;
			case BrOpen:
				result = token.access().firstOf(BrClose).token;
			case BkOpen:
				result = token.access().firstOf(BkClose).token;
			default:
				return null;
		}
		if (result == null) {
			return null;
		}
		closeTokenCache.set(token.index, result);
		return result;
	}

	public function getTokenAtOffset(off:Int):Null<TokenInfo> {
		if (tokens.length <= 0) {
			throw BAD_OFFSET;
		}
		if (off < 0) {
			throw BAD_OFFSET;
		}
		if (off > tokens[tokens.length - 1].token.pos.max) {
			throw BAD_OFFSET;
		}

		for (token in tokens) {
			if (token == null) {
				continue;
			}
			if (token.token.pos.max >= off) {
				return token;
			}
		}
		throw BAD_OFFSET;
	}

	public function getTokenAt(index:Int):Null<TokenInfo> {
		if ((index < 0) || (index >= tokens.length)) {
			return null;
		}
		return tokens[index];
	}

	public function getPreviousToken(token:TokenTree):Null<TokenInfo> {
		if ((token == null) || (token.index <= 0)) {
			return null;
		}
		var prevToken:Null<TokenInfo> = null;
		var prevIndex:Int = token.index - 1;
		if (prevIndex >= tokens.length) {
			return null;
		}
		while (prevToken == null) {
			prevToken = tokens[prevIndex--];
			if (prevIndex < 0) {
				return null;
			}
		}
		return prevToken;
	}

	public function getNextToken(token:TokenTree):Null<TokenInfo> {
		if ((token == null) || (token.index <= 0)) {
			return null;
		}
		var nextToken:Null<TokenInfo> = null;
		var nextIndex:Int = token.index + 1;
		if (nextIndex >= tokens.length) {
			return null;
		}
		while (nextToken == null) {
			nextToken = tokens[nextIndex++];
			if (nextIndex < 0) {
				return null;
			}
		}
		return nextToken;
	}

	public function whitespace(token:TokenTree, where:WhitespacePolicy, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		var prev:Null<TokenInfo> = null;
		var prevIndex:Int = token.index - 1;
		while (prev == null) {
			if (prevIndex < 0) {
				break;
			}
			prev = tokens[prevIndex--];
		}
		if ((prev != null) && needsLineBreak(prev.token)) {
			prev = null;
		}
		#if debugLog
		logAction(pos, token, '$where');
		#end
		switch (where) {
			case None:
				applyWhitespace(info, None, pos);
				applyWhitespace(prev, None, pos);
			case NoneAfter:
				applyWhitespace(info, None, pos);
			case OnlyAfter:
				applyWhitespace(prev, None, pos);
				applyWhitespace(info, Space, pos);
			case Before:
				applyWhitespace(prev, Space, pos);
			case NoneBefore:
				applyWhitespace(prev, None, pos);
			case OnlyBefore:
				applyWhitespace(prev, Space, pos);
				applyWhitespace(info, None, pos);
			case After:
				applyWhitespace(info, Space, pos);
			case Around:
				applyWhitespace(info, Space, pos);
				applyWhitespace(prev, Space, pos);
		}
	}

	function applyWhitespace(info:Null<TokenInfo>, policy:WhitespaceAfterType, ?pos:PosInfos) {
		if (info == null) {
			return;
		}
		#if debugLog
		var oldWhitespaceafter:WhitespaceAfterType = info.whitespaceAfter;
		#end
		switch (info.whitespaceAfter) {
			case None:
				info.whitespaceAfter = policy;
				switch (policy) {
					case None:
						info.spacesAfter = 0;
					case Space:
						shouldHaveOneSpaceAfter(info);
					case Newline:
				}
			case Space:
				switch (policy) {
					case None:
						info.whitespaceAfter = None;
						info.spacesAfter = 0;
					case Space:
						shouldHaveOneSpaceAfter(info);
					case Newline:
						info.whitespaceAfter = policy;
				}
			case Newline:
				switch (policy) {
					case None:
						if (info.spacesAfter > 0) {
							info.whitespaceAfter = Space;
						} else {
							info.whitespaceAfter = None;
							info.spacesAfter = 0;
						}
					case Space:
						info.whitespaceAfter = policy;
						shouldHaveOneSpaceAfter(info);
					case Newline:
				}
		}
		#if debugLog
		logAction(pos, info.token, '$oldWhitespaceafter -> ${info.whitespaceAfter}');
		#end
	}

	function shouldHaveOneSpaceAfter(info:TokenInfo) {
		if (info == null) {
			return;
		}
		if (info.spacesAfter <= 0) {
			info.spacesAfter = 1;
		}
	}

	public function spacesAfter(token:TokenTree, count:Int, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.spacesAfter} -> $count');
		#end
		info.spacesAfter = count;
		if ((info.whitespaceAfter == None) && (count > 0)) {
			#if debugLog
			logAction(pos, info.token, '${info.whitespaceAfter} -> Space');
			#end
			info.whitespaceAfter = Space;
		}
	}

	public function spacesBefore(token:TokenTree, count:Int, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.spacesBefore} -> $count');
		#end
		info.spacesBefore = count;
	}

	public function lineEndAfter(token:TokenTree, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}

		switch (info.whitespaceAfter) {
			case None:
				#if debugLog
				logAction(pos, token, "None -> NewLine");
				#end
				info.whitespaceAfter = Newline;
			case Newline:
			case Space:
				#if debugLog
				logAction(pos, token, "Space -> Newline");
				#end
				info.whitespaceAfter = Newline;
		}
	}

	public function lineEndBefore(token:TokenTree, ?pos:PosInfos) {
		var info:Null<TokenInfo> = getPreviousToken(token);
		if (info == null) {
			return;
		}
		switch (info.whitespaceAfter) {
			case None:
				#if debugLog
				logAction(pos, token, "None -> NewLine");
				#end
				info.whitespaceAfter = Newline;
			case Newline:
			case Space:
				#if debugLog
				logAction(pos, token, "Space -> SpaceOrNewline");
				#end
				info.whitespaceAfter = Newline;
		}
	}

	function needsLineBreak(token:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		switch (token.tok) {
			case CommentLine(_):
				return true;
			default:
		}
		return false;
	}

	public function noLineEndAfter(token:TokenTree, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		if (needsLineBreak(info.token)) {
			#if debugLog
			logAction(pos, token, '${info.whitespaceAfter} -> Newline');
			#end
			info.whitespaceAfter = Newline;
			return;
		}
		switch (info.whitespaceAfter) {
			case None:
				return;
			case Space:
			case Newline:
				if (info.spacesAfter <= 0) {
					#if debugLog
					logAction(pos, token, NEWLINE_TO_SPACE);
					#end
					info.whitespaceAfter = None;
				} else {
					#if debugLog
					logAction(pos, token, NEWLINE_TO_SPACE);
					#end
					info.whitespaceAfter = Space;
					shouldHaveOneSpaceAfter(info);
				}
		}
	}

	public function noLineEndBefore(token:TokenTree, ?pos:PosInfos) {
		var info:Null<TokenInfo> = getPreviousToken(token);
		if (info == null) {
			return;
		}
		if (needsLineBreak(info.token)) {
			#if debugLog
			logAction(pos, token, '${info.whitespaceAfter} -> Newline');
			#end
			info.whitespaceAfter = Newline;
			return;
		}
		switch (info.whitespaceAfter) {
			case None:
				return;
			case Space:
			case Newline:
				if (info.spacesAfter <= 0) {
					#if debugLog
					logAction(pos, token, NEWLINE_TO_SPACE);
					#end
					info.whitespaceAfter = None;
				} else {
					#if debugLog
					logAction(pos, token, NEWLINE_TO_SPACE);
					#end
					info.whitespaceAfter = Space;
					shouldHaveOneSpaceAfter(info);
				}
		}
	}

	public function emptyLinesAfter(token:TokenTree, count:Int, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.emptyLinesAfter} -> $count');
		#end
		info.emptyLinesAfter = count;
	}

	public function emptyLinesBefore(token:TokenTree, count:Int, ?pos:PosInfos) {
		if (token.index <= 0) {
			#if debugLog
			logAction(pos, token, '$leadingEmptyLInes -> $count');
			#end
			leadingEmptyLInes = count;
			return;
		}
		var info:Null<TokenInfo> = getPreviousToken(token);
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.emptyLinesAfter} -> $count');
		#end
		info.emptyLinesAfter = count;
	}

	public function emptyLinesAfterSubTree(token:TokenTree, count:Int, ?pos:PosInfos) {
		var lastToken:TokenTree = TokenTreeCheckUtils.getLastToken(token);
		if (lastToken == null) {
			return;
		}
		var info:Null<TokenInfo> = tokens[lastToken.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, lastToken, '${info.emptyLinesAfter} -> $count');
		#end
		info.emptyLinesAfter = count;
	}

	public function tokenText(token:TokenTree, text:String, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		info.text = text;
	}

	public function wrapAfter(token:TokenTree, wrap:Bool, ?pos:PosInfos) {
		if (token.index < 0) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.wrapAfter} -> $wrap');
		#end
		info.wrapAfter = wrap;
	}

	public function wrapBefore(token:TokenTree, wrap:Bool, ?pos:PosInfos) {
		var prev:Null<TokenInfo> = getPreviousToken(token);
		if (prev == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${prev.wrapAfter} -> $wrap');
		#end
		prev.wrapAfter = wrap;
	}

	public function noWrappingBetween(tokenStart:TokenTree, tokenEnd:TokenTree, config:Config, allowCommas:Bool = true, ?pos:PosInfos) {
		if ((tokenStart == null) || (tokenEnd == null)) {
			return;
		}
		var index = tokenStart.index;
		while (index < tokenEnd.index) {
			var first:Bool = index == tokenStart.index;
			var info:Null<TokenInfo> = tokens[index++];
			var next:Null<TokenInfo> = tokens[index];
			if (info == null) {
				continue;
			}
			switch (info.token.tok) {
				case POpen:
					if (!first) {
						var close:Null<TokenTree> = getCloseToken(info.token);
						if (close != null) {
							index = close.index;
							continue;
						}
					}
				case BrOpen:
					if (!first) {
						var close:Null<TokenTree> = getCloseToken(info.token);
						if (close != null) {
							index = close.index;
							continue;
						}
					}
				case BkOpen:
					if (!first) {
						var close:Null<TokenTree> = getCloseToken(info.token);
						if (close != null) {
							index = close.index;
							continue;
						}
					}
				case Comma:
					if (allowCommas) {
						continue;
					}
				case CommentLine(_):
					continue;
				case Comment(_):
					continue;
				case Kwd(KwdFunction) | Kwd(KwdMacro) | Arrow:
					var lastChild:TokenTree = TokenTreeCheckUtils.getLastToken(info.token);
					if (lastChild != null) {
						if (lastChild.index > index) {
							index = lastChild.index;
						}
						continue;
					}
				default:
			}
			info.wrapAfter = false;
			if (next != null) {
				switch (next.token.tok) {
					case BrOpen:
						switch (config.lineEnds.leftCurly) {
							case Before, Both:
								continue;
							case None, After:
						}
					default:
				}
			}
			#if debugLog
			var oldWhitespaceAfter:WhitespaceAfterType = info.whitespaceAfter;
			#end
			switch (info.whitespaceAfter) {
				case None:
				case Space:
				case Newline:
					if (info.spacesAfter <= 0) {
						info.whitespaceAfter = None;
					} else {
						info.whitespaceAfter = Space;
						shouldHaveOneSpaceAfter(info);
					}
			}
			#if debugLog
			logAction(pos, info.token, '$oldWhitespaceAfter -> ${info.whitespaceAfter}');
			#end
		}
	}

	public function additionalIndent(token:TokenTree, indent:Null<Int>, ?pos:PosInfos) {
		if ((indent == null) || (token == null) || (token.index < 0)) {
			return;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return;
		}
		#if debugLog
		logAction(pos, token, '${info.additionalIndent} -> $indent');
		#end
		info.additionalIndent = indent;
	}

	public function increaseIndentBetween(start:Null<TokenTree>, end:Null<TokenTree>, depth:Int, config:Config, parsedCode:ParsedCode, indenter:Indenter,
			?pos:PosInfos) {
		if ((depth == 0) || (start == null) || (start.index < 0) || (end == null) || (end.index < 0)) {
			return;
		}
		var startIndex:Int = start.index;
		if (depth > 0) {
			startIndex++;
		}
		var endIndex:Int = end.index;
		var outputLineEnds:String = MarkLineEnds.outputLineSeparator(config.lineEnds, parsedCode);
		for (index in startIndex...endIndex) {
			var info:Null<TokenInfo> = tokens[index];
			if (info == null) {
				continue;
			}
			#if debugLog
			logAction(pos, info.token, '${info.additionalIndent} -> ${info.additionalIndent + depth}');
			#end
			info.additionalIndent += depth;
			switch (info.token.tok) {
				case Comment(s):
					var lines:Array<String> = info.text.split(outputLineEnds);
					var addIndent:String = indenter.makeIndentString(depth);
					if (lines.length > 0) {
						for (line in 1...lines.length) {
							lines[line] = addIndent + lines[line];
						}
						info.text = lines.join(outputLineEnds);
					}
				default:
			}
		}
	}

	public function findTokenAtOffset(offset:Int):Null<TokenInfo> {
		var lastInfo:Null<TokenInfo> = null;

		for (info in tokens) {
			if (info == null) {
				continue;
			}
			if (info.token.pos.min >= offset) {
				return lastInfo;
			}
			lastInfo = info;
		}
		return lastInfo;
	}

	public function isSameLine(first:TokenTree, second:TokenTree):Bool {
		var startIndex:Int = first.index;
		var endIndex:Int = second.index;
		if (startIndex == endIndex) {
			return true;
		}
		if ((startIndex < 0) || (endIndex < 0)) {
			return false;
		}

		if (startIndex > endIndex) {
			startIndex = second.index;
			endIndex = first.index;
		}
		while (startIndex < endIndex) {
			var currTok:Null<TokenInfo> = tokens[startIndex++];
			if (currTok == null) {
				continue;
			}
			if (currTok.whitespaceAfter == Newline) {
				return false;
			}
		}
		return true;
	}

	public function findLowestIndex(start:TokenTree):TokenTree {
		var lowest:TokenTree = start;
		if (!start.hasChildren()) {
			return lowest;
		}
		for (child in start.children) {
			var token:TokenTree = findLowestIndex(child);
			if (token.index < lowest.index) {
				lowest = token;
			}
		}
		return lowest;
	}

	public function calcLength(token:TokenTree):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var current:Null<TokenInfo> = tokens[token.index];
		if (current == null) {
			return 0;
		}
		var spaceAdd:Int = 0;
		if (current.whitespaceAfter == Space) {
			spaceAdd = 1;
		}
		if (current.text == null) {
			current.text = '${current.token}';
		}
		var length:Int = current.text.length + spaceAdd;
		if ((token.children == null) || (token.children.length <= 0)) {
			return length;
		}
		for (child in token.children) {
			length += calcLength(child);
		}
		return length;
	}

	public function isMultilineToken(token:TokenTree):Bool {
		if ((token == null) || (token.index < 0)) {
			return false;
		}
		var current:Null<TokenInfo> = tokens[token.index];
		if (current == null) {
			return false;
		}
		if ((current.text.indexOf("\r") >= 0) || (current.text.indexOf("\n") >= 0)) {
			return true;
		}
		return false;
	}

	public function calcLengthUntilNewline(token:TokenTree, stop:Null<TokenTree>):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var endIndex:Int = tokens.length - 1;
		if (stop != null) {
			endIndex = stop.index;
		}
		var index:Int = token.index;
		var length:Int = 0;
		while (index <= endIndex) {
			var current:Null<TokenInfo> = tokens[index++];
			if (current == null) {
				continue;
			}
			if (current.text == null) {
				current.text = '${current.token}';
			}
			if (current.text.indexOf("\r") >= 0) {
				length += current.text.indexOf("\r");
				break;
			}
			if (current.text.indexOf("\n") >= 0) {
				length += current.text.indexOf("\n");
				break;
			}
			length += current.spacesBefore;
			if (current.whitespaceAfter == Space) {
				length += current.spacesAfter;
			}
			length += current.text.length;
			if (current.whitespaceAfter == Newline) {
				break;
			}
		}
		return length;
	}

	public function calcLengthBetween(tokenStart:TokenTree, tokenEnd:TokenTree):Int {
		if ((tokenStart == null) || (tokenEnd == null)) {
			return 0;
		}
		if ((tokenStart.index < 0) || (tokenEnd.index < 0)) {
			return 0;
		}
		var length:Int = 0;
		for (index in tokenStart.index...tokenEnd.index) {
			var current:Null<TokenInfo> = tokens[index];
			if (current == null) {
				continue;
			}
			if (current.text == null) {
				current.text = '${current.token}';
			}
			length += current.text.length;
			if (current.whitespaceAfter == Space) {
				length++;
			}
		}
		return length;
	}

	public function calcLineLength(token:TokenTree):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var start:Int = token.index - 1;
		while (true) {
			if (start < 0) {
				break;
			}
			var info:Null<TokenInfo> = tokens[start--];
			if (info == null) {
				continue;
			}
			if (info.whitespaceAfter == Newline) {
				start += 2;
				break;
			}
		}
		var length:Int = 0;
		while (true) {
			if (start >= tokens.length) {
				break;
			}
			var info:Null<TokenInfo> = tokens[start++];
			if (info == null) {
				continue;
			}
			length += info.text.length;
			switch (info.whitespaceAfter) {
				case None:
				case Space:
					length += info.spacesAfter;
				case Newline:
					break;
			}
		}
		return length;
	}

	public function calcLineLengthBefore(token:TokenTree):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var start:Int = token.index - 1;
		var length:Int = 0;
		while (true) {
			if (start < 0) {
				break;
			}
			var info:Null<TokenInfo> = tokens[start--];
			if (info == null) {
				continue;
			}
			switch (info.whitespaceAfter) {
				case None:
				case Space:
					length += info.spacesAfter;
				case Newline:
					break;
			}
			length += info.text.length;
		}
		return length;
	}

	public function calcLineLengthAfter(token:TokenTree):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var start:Int = token.index + 1;
		var length:Int = 0;
		while (true) {
			if (start >= tokens.length) {
				break;
			}
			var info:Null<TokenInfo> = tokens[start++];
			if (info == null) {
				continue;
			}
			var tokenLength = info.text.length;
			var linefeed:Int = info.text.lastIndexOf("\r");
			if (linefeed >= 0) {
				tokenLength -= linefeed;
			}
			linefeed = info.text.lastIndexOf("\n");
			if (linefeed >= 0) {
				tokenLength -= linefeed;
			}
			length += tokenLength;
			switch (info.whitespaceAfter) {
				case None:
				case Space:
					length += info.spacesAfter;
				case Newline:
					break;
			}
		}
		return length;
	}

	public function calcTokenLength(token:TokenTree):Int {
		if ((token == null) || (token.index < 0)) {
			return 0;
		}
		var info:Null<TokenInfo> = tokens[token.index];
		if (info == null) {
			return 0;
		}
		var length:Int = info.text.length;
		switch (info.whitespaceAfter) {
			case None:
			case Space:
				length += info.spacesAfter;
			case Newline:
		}
		return length;
	}

	public function isNewLineBefore(token:TokenTree):Bool {
		var info:Null<TokenInfo> = getPreviousToken(token);
		if (info == null) {
			return false;
		}
		return (info.whitespaceAfter == Newline);
	}

	public function isNewLineAfter(token:TokenTree):Bool {
		var info:Null<TokenInfo> = getTokenAt(token.index);
		if (info == null) {
			return false;
		}
		return (info.whitespaceAfter == Newline);
	}

	// public function calcNewLinesBetween(tokenStart:TokenTree, tokenEnd:TokenTree):Int {
	// 	if ((tokenStart == null) || (tokenEnd == null)) {
	// 		return 0;
	// 	}
	// 	if ((tokenStart.index < 0) || (tokenEnd.index < 0)) {
	// 		return 0;
	// 	}
	// 	var count:Int = 0;
	// 	for (index in tokenStart.index...tokenEnd.index) {
	// 		var current:Null<TokenInfo> = tokens[index];
	// 		if (current == null) {
	// 			continue;
	// 		}
	// 		if (current.whitespaceAfter == Newline) {
	// 			count++;
	// 		}
	// 	}
	// 	return count;
	// }

	public function isSameLineBetween(tokenStart:TokenTree, tokenEnd:TokenTree, exclude:Bool):Bool {
		if ((tokenStart == null) || (tokenEnd == null)) {
			return true;
		}
		var start:Int = tokenStart.index;
		var end:Int = tokenEnd.index;
		if (exclude) {
			start++;
		}
		for (index in start...end) {
			var info:Null<TokenInfo> = tokens[index];
			if (info == null) {
				continue;
			}
			if (info.whitespaceAfter == Newline) {
				return false;
			}
			if (info.text == null) {
				continue;
			}
			if ((info.text.indexOf("\r") >= 0) || (info.text.indexOf("\n") >= 0)) {
				return false;
			}
		}
		return true;
	}

	public function findLineStartToken(token:Null<TokenTree>):Null<TokenTree> {
		if ((token == null) || (token.index < 0)) {
			return null;
		}
		var start:Int = token.index - 1;
		while (true) {
			if (start < 0) {
				return tokens[0].token;
			}
			var info:Null<TokenInfo> = tokens[start--];
			if (info == null) {
				continue;
			}
			switch (info.whitespaceAfter) {
				case None:
				case Space:
				case Newline:
					return token;
			}
			token = info.token;
		}
		return null;
	}

	#if debugLog
	function logFileStart() {
		#if !js
		var file:FileOutput = File.append("hxformat.log", false);
		file.writeString("\n\n".lpad("-", 202) + "\n".lpad("-", 201));
		file.close();
		#end
	}

	function logAction(callerPos:PosInfos, token:TokenTree, what:String, ?pos:PosInfos) {
		#if !js
		var func:String = '${callerPos.fileName}:${callerPos.lineNumber}:${callerPos.methodName}';
		var operation:String = '${pos.methodName.rpad(" ", 25)} $what';
		var tok:String = '`${token}`';
		var tokPos:String = '${tok.rpad(" ", 30)} (${token.pos})';

		var text:String = func.rpad(" ", 80) + operation.rpad(" ", 70) + tokPos;
		var file:FileOutput = File.append("hxformat.log", false);
		file.writeString(text + "\n");
		file.close();
		#end
	}
	#end
}
