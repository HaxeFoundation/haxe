package formatter.codedata;

import formatter.codedata.FormatterInputData;
import formatter.config.WrapConfig;
import formatter.marker.Indenter;

class CodeLines {
	static inline var FORMATTER_OFF:String = " @formatter:off";
	static inline var FORMATTER_ON:String = " @formatter:on";

	var indenter:Indenter;
	var parsedCode:ParsedCode;
	var range:FormatterInputRange;
	var posRange:FormatterInputRange;
	var trailingWhitespaceAfterRange:String;
	var rangeStartOffset:Int;
	var rangeEndOffset:Int;

	public var lines(default, null):Array<CodeLine>;

	public function new(parsedCode:ParsedCode, indenter:Indenter, ?range:FormatterInputRange) {
		lines = [];
		this.indenter = indenter;
		this.parsedCode = parsedCode;
		this.range = null;
		this.posRange = range;
		this.trailingWhitespaceAfterRange = "";
		this.rangeStartOffset = 0;
		this.rangeEndOffset = 0;
		if (range != null) {
			var start:Null<TokenInfo> = parsedCode.tokenList.getTokenAtOffset(range.startPos);
			var end:Null<TokenInfo> = parsedCode.tokenList.getTokenAtOffset(range.endPos);
			if ((end != null) && (range.endPos - 1 < end.token.pos.min)) {
				end = parsedCode.tokenList.getPreviousToken(end.token);
			}
			if ((start != null) && (end != null)) {
				switch (start.token.tok) {
					case Comment(s):
						rangeStartOffset = calcStartCommentOffset(start);
					default:
						var startLine:LinePos = parsedCode.getLinePos(start.token.pos.min);
						var rangeStartLine:LinePos = parsedCode.getLinePos(range.startPos);
						var endLine:LinePos = parsedCode.getLinePos(start.token.pos.max);

						if (startLine.line != rangeStartLine.line) {
							rangeStartOffset = range.startPos - rangeStartLine.ofs - start.token.pos.min;
						}
				}
				switch (end.token.tok) {
					case Comment(s):
						rangeEndOffset = calcEndCommentOffset(end);
					default:
						if ((posRange.endPos >= end.token.pos.min) && (posRange.endPos < end.token.pos.max)) {
							rangeEndOffset = end.text.length - (end.token.pos.max - posRange.endPos);
						}
				}

				this.range = {startPos: start.token.index, endPos: end.token.index};
			}
		}
		buildLines();
	}

	function calcStartCommentOffset(info:TokenInfo):Int {
		var comment:String = parsedCode.getString(info.token.pos.min, info.token.pos.max);
		var commentLines:Array<String> = comment.split(parsedCode.lineSeparator);

		var index:Int = 0;
		var offset:Int = 0;
		var sepLength:Int = parsedCode.lineSeparator.length;
		var pos:Int = posRange.startPos - info.token.pos.min;
		for (line in commentLines) {
			if (line.length + offset + sepLength > pos) {
				break;
			}
			offset += line.length + sepLength;
			index++;
		}
		commentLines = info.text.split(parsedCode.lineSeparator);

		offset = 0;
		for (j in 0...index) {
			offset += commentLines[j].length + sepLength;
		}
		return offset;
	}

	function calcEndCommentOffset(info:TokenInfo):Int {
		var comment:String = parsedCode.getString(info.token.pos.min, info.token.pos.max);
		var commentLines:Array<String> = comment.split(parsedCode.lineSeparator);

		var index:Int = 0;
		var offset:Int = 0;
		var trailCount:Int = 0;
		var pos:Int = posRange.endPos - info.token.pos.min;
		var sepLength:Int = parsedCode.lineSeparator.length;
		for (line in commentLines) {
			if (line.length + offset + sepLength > pos) {
				trailCount = line.length - (pos - offset);
				break;
			}
			offset += line.length + sepLength;
			index++;
		}
		commentLines = info.text.split(parsedCode.lineSeparator);
		offset = 0;
		for (j in 0...index) {
			offset += commentLines[j].length + sepLength;
		}
		var lastLine:String = commentLines[index];
		return offset + lastLine.length - trailCount;
	}

	function buildLines() {
		var line:Null<CodeLine> = null;
		var index:Int = 0;
		while (index < parsedCode.tokenList.tokens.length) {
			if (range != null) {
				if (index > range.endPos) {
					break;
				}
			}
			var tokenInfo:TokenInfo = parsedCode.tokenList.getTokenAt(index);
			if (tokenInfo == null) {
				index++;
				continue;
			}
			switch (tokenInfo.token.tok) {
				case CommentLine(FORMATTER_OFF):
					line = null;
					index = skipFormatterOff(index);
					continue;
				default:
			}
			if (range != null) {
				if (index < range.startPos) {
					index++;
					continue;
				}
			}
			if (line == null) {
				line = new CodeLine(indenter.calcIndent(tokenInfo.token) + tokenInfo.additionalIndent);
				lines.push(line);
			}
			if (range != null) {
				if ((index == range.startPos) && (rangeStartOffset > 0)) {
					tokenInfo.text = tokenInfo.text.substr(rangeStartOffset);
					line.indent = 0;
				}
			}
			if ((range != null) && (index == range.endPos)) {
				if ((posRange.endPos >= tokenInfo.token.pos.min) && (posRange.endPos < tokenInfo.token.pos.max)) {
					var index:Int = 0;
					if (range.endPos == range.startPos) {
						index = rangeStartOffset;
					}
					tokenInfo.text = tokenInfo.text.substr(0, rangeEndOffset - index);
					tokenInfo.spacesAfter = 0;
					tokenInfo.whitespaceAfter = None;
					tokenInfo.emptyLinesAfter = 0;
					line.partialLine = true;
				}
				if (posRange.endPos > tokenInfo.token.pos.max) {
					trailingWhitespaceAfterRange = parsedCode.getString(tokenInfo.token.pos.max, posRange.endPos);
					line.partialLine = true;
				}
			}
			line.addToken(tokenInfo);
			if (tokenInfo.whitespaceAfter == Newline) {
				line = null;
			}
			index++;
		}
	}

	function skipFormatterOff(index:Int):Int {
		var startIndex:Int = index++;
		var startInfo:TokenInfo = parsedCode.tokenList.getTokenAt(startIndex);
		var startLine:Int = parsedCode.getLinePos(startInfo.token.pos.min).line;

		while (index < parsedCode.tokenList.tokens.length) {
			var endIndex:Int = index++;
			var tokenInfo:TokenInfo = parsedCode.tokenList.getTokenAt(endIndex);
			if (tokenInfo == null) {
				continue;
			}
			if (range != null) {
				if (endIndex < range.startPos) {
					continue;
				}
				if (endIndex == range.startPos) {
					startInfo = parsedCode.tokenList.getTokenAt(endIndex);
					startLine = parsedCode.getLinePos(startInfo.token.pos.min).line;
				}
				if (endIndex >= range.endPos) {
					copyVerbatimChars(startInfo.token.pos.min, tokenInfo.token.pos.max);
					return index;
				}
			}
			switch (tokenInfo.token.tok) {
				case CommentLine(FORMATTER_ON):
					if (range != null) {
						if (startIndex < range.startPos) {
							copyVerbatimChars(startInfo.token.pos.min, tokenInfo.token.pos.max);
							return index;
						}
					}
					var endLine:Int = parsedCode.getLinePos(tokenInfo.token.pos.max).line;
					copyVerbatimLines(startLine, endLine);
					return index;
				default:
			}
		}
		var endLine:Int = parsedCode.lines.length - 1;
		copyVerbatimLines(startLine, endLine);
		return index;
	}

	function copyVerbatimLines(startLine:Int, endLine:Int) {
		var startOffs:Int = parsedCode.linesIdx[startLine].l;
		var endOffs:Int = parsedCode.linesIdx[endLine].r;
		var content:String = parsedCode.getString(startOffs, endOffs);
		if (endLine < parsedCode.lines.length - 1) {
			content = content.rtrim();
		}
		lines.push(new VerbatimCodeLine(content));
	}

	function copyVerbatimChars(startPos:Int, endPos:Int) {
		var content:String = parsedCode.getString(startPos, endPos);
		lines.push(new VerbatimCodeLine(content));
	}

	public function applyWrapping(config:WrapConfig, lineSeparator:String) {
		var wrappedLines:Array<CodeLine> = [];

		for (line in lines) {
			var wrappedCode:Array<CodeLine> = line.applyWrapping(config, parsedCode, indenter, lineSeparator);
			wrappedLines = wrappedLines.concat(wrappedCode);
		}
		lines = wrappedLines;
	}

	public function print(lineSeparator:String):String {
		var prefix:String = "";
		if (parsedCode.tokenList.leadingEmptyLInes > 0) {
			prefix = "".lpad(lineSeparator, lineSeparator.length * parsedCode.tokenList.leadingEmptyLInes);
		}
		if (range != null) {
			if (range.startPos > 0) {
				prefix = "";
			}
			if (lines.length > 0) {
				var line:CodeLine = lines[lines.length - 1];
				line.emptyLinesAfter = 0;
			}
		}
		return prefix + lines.map(function(line) return line.print(indenter, lineSeparator)).join(lineSeparator) + trailingWhitespaceAfterRange;
	}
}
