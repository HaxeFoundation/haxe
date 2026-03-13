package formatter.codedata;

import formatter.config.WrapConfig;
import formatter.marker.Indenter;

class CodeLine {
	var parts:Array<CodePart>;
	var currentPart:Null<CodePart>;

	public var indent:Int;
	public var emptyLinesAfter:Int;
	public var verbatim:Bool;
	public var partialLine:Bool;

	public function new(indent:Int) {
		this.indent = indent;
		parts = [];
		emptyLinesAfter = 0;
		verbatim = false;
		partialLine = false;
	}

	public function addToken(tokenInfo:TokenInfo) {
		if (tokenInfo.emptyLinesAfter > emptyLinesAfter) {
			emptyLinesAfter = tokenInfo.emptyLinesAfter;
		}
		if (currentPart == null) {
			currentPart = {
				firstToken: tokenInfo.token,
				lastToken: tokenInfo.token,
				text: "",
				multiLine: false,
				firstLineLength: -1,
				lastLineLength: -1
			};
			parts.push(currentPart);
		}
		currentPart.lastToken = tokenInfo.token;
		if (tokenInfo.spacesBefore > 0) {
			currentPart.text += "".lpad(" ", tokenInfo.spacesBefore);
		}
		if ((tokenInfo.whitespaceAfter == Space) && (tokenInfo.spacesAfter > 0)) {
			currentPart.text += tokenInfo.text + "".lpad(" ", tokenInfo.spacesAfter);
		} else {
			currentPart.text += tokenInfo.text;
		}
		if (tokenInfo.wrapAfter) {
			currentPart = null;
		}
	}

	public function applyWrapping(config:WrapConfig, parsedCode:ParsedCode, indenter:Indenter, lineSeparator:String):Array<CodeLine> {
		var lineLength:Int = indenter.calcAbsoluteIndent(indent);
		for (part in parts) {
			calcLineLengths(part, lineSeparator);
		}
		for (index in 0...parts.length) {
			var part:CodePart = parts[index];
			if (part.multiLine) {
				if (lineLength + part.firstLineLength > config.maxLineLength) {
					return wrappedAt(index, config, parsedCode, indenter);
				}
				lineLength = part.lastLineLength;
				continue;
			}
			lineLength += part.text.length;
			if (lineLength > config.maxLineLength) {
				return wrappedAt(index, config, parsedCode, indenter);
			}
		}
		return [this];
	}

	function calcLineLengths(part:CodePart, lineSeparator:String) {
		var lines:Array<String> = part.text.split(lineSeparator);
		part.multiLine = (lines.length > 1);
		if (part.multiLine) {
			part.firstLineLength = lines[0].length;
			part.lastLineLength = lines[lines.length - 1].length;
		} else {
			part.firstLineLength = part.text.length;
			part.lastLineLength = -1;
		}
	}

	function wrappedAt(index:Int, config:WrapConfig, parsedCode:ParsedCode, indenter:Indenter):Array<CodeLine> {
		return wrapNormal(config, parsedCode, indenter);
	}

	function wrapNormal(config:WrapConfig, parsedCode:ParsedCode, indenter:Indenter):Array<CodeLine> {
		if (parts.length <= 0) {
			return [this];
		}
		var line:CodeLine = new CodeLine(indent);
		var part:CodePart = parts.shift();
		line.parts = [part];
		var lineLength:Int = indenter.calcAbsoluteIndent(indent) + part.text.length;
		var lines:Array<CodeLine> = [line];
		var lastPart:CodePart = part;
		while (parts.length > 0) {
			var p:CodePart = parts.shift();
			var partLength:Int = p.firstLineLength;
			if (!p.multiLine) {
				partLength = p.text.rtrim().length;
			}
			if (lineLength + partLength >= config.maxLineLength) {
				parsedCode.tokenList.lineEndAfter(lastPart.lastToken);
				var info:TokenInfo = parsedCode.tokenList.getTokenAt(p.firstToken.index);
				var additionalIndent:Int = 0;
				if (info != null) {
					additionalIndent = info.additionalIndent;
				}
				var newIndent:Int = indenter.calcIndent(p.firstToken) + additionalIndent;
				line = new CodeLine(newIndent);
				lineLength = indenter.calcAbsoluteIndent(newIndent);
				lines.push(line);
			}
			line.parts.push(p);
			if (p.lastLineLength <= 0) {
				lineLength += p.firstLineLength;
			} else {
				lineLength += p.lastLineLength;
			}
			lastPart = p;
		}
		line.emptyLinesAfter = emptyLinesAfter;
		return lines;
	}

	public function print(indenter:Indenter, lineSeparator:String):String {
		var line:String = "";
		for (part in parts) {
			line += part.text;
		}
		if (partialLine) {
			line = indenter.makeIndentString(indent) + line;
		} else {
			line = indenter.makeIndentString(indent) + line.rtrim();
		}
		for (index in 0...emptyLinesAfter) {
			line += lineSeparator;
			if (indenter.shouldAddTrailingWhitespace()) {
				line += indenter.makeIndentString(indent);
			}
		}
		return line;
	}
}

typedef CodePart = {
	var firstToken:TokenTree;
	var lastToken:TokenTree;
	var text:String;
	var multiLine:Bool;
	var firstLineLength:Int;
	var lastLineLength:Int;
}
