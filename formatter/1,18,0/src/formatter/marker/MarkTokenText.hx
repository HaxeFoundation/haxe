package formatter.marker;

import haxeparser.HaxeLexer;
import tokentree.TokenStream;
import tokentree.TokenStreamProgress;
import tokentree.walk.WalkStatement;
import formatter.codedata.CodeLines;
import formatter.codedata.FormatterInputData;
import formatter.codedata.ParsedCode;

class MarkTokenText extends MarkerBase {
	public function run() {
		parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Const(CString(text)):
					tokenText(token, printStringToken(token));
				case Const(CRegexp(_, _)):
					tokenText(token, printEregToken(token));
				case CommentLine(text):
					tokenText(token, printCommentLine(text));
				default:
					tokenText(token, token.toString());
			}
			return GoDeeper;
		});
	}

	public function finalRun() {
		parsedCode.root.filterCallback(function(token:TokenTree, index:Int):FilterResult {
			switch (token.tok) {
				case Comment(text):
					tokenText(token, printComment(text, token));
				default:
			}
			return GoDeeper;
		});
	}

	public function printStringToken(token:TokenTree):String {
		var text:String = parsedCode.getString(token.pos.min, token.pos.max);
		if (!config.whitespace.formatStringInterpolation) {
			return text;
		}
		if (text.startsWith("'")) {
			var start:Int = 0;
			var index:Int;
			while ((index = text.indexOf("${", start)) >= 0) {
				if (isDollarEscaped(text, index)) {
					return text;
				}
				start = index + 1;
				var indexEnd:Int = text.indexOf("}", index + 2);
				var fragment:String = text.substring(index + 2, indexEnd);
				if (fragment.indexOf("{") >= 0) {
					continue;
				}
				var formatted:String = formatFragment(fragment); // , tokenIndent);
				start += formatted.length;
				text = text.substr(0, index + 2) + formatted + text.substr(indexEnd);
			}
		}
		return text;
	}

	function isDollarEscaped(text:String, index:Int):Bool {
		var escaped:Bool = false;
		while (--index >= 0) {
			if (text.fastCodeAt(index) != "$".code) {
				return escaped;
			}
			escaped = !escaped;
		}
		return escaped;
	}

	function formatFragment(fragment:String):String {
		try {
			var fileName:String = "string interpolation";
			var tokens:Array<Token> = makeTokens(ByteData.ofString(fragment), fileName);
			var stream:TokenStream = new TokenStream(tokens, ByteData.ofString(fragment));
			var root:TokenTree = new TokenTree(Root, "", null, -1);
			var progress:TokenStreamProgress = new TokenStreamProgress(stream);
			while (progress.streamHasChanged()) {
				if (stream.hasMore()) {
					WalkStatement.walkStatement(stream, root);
				}
			}

			var inputData:FormatterInputData = {
				fileName: fileName,
				content: cast ByteData.ofString(fragment),
				tokenList: tokens,
				tokenTree: root,
				config: config,
				entryPoint: ExpressionLevel
			};
			var interpolParsedCode:ParsedCode = new ParsedCode(inputData);
			var interpolIndenter = new Indenter(config.indentation);
			interpolIndenter.setParsedCode(interpolParsedCode);

			var markTokenText:MarkTokenText = new MarkTokenText(config, interpolParsedCode, interpolIndenter);
			var markWhitespace:MarkWhitespace = new MarkWhitespace(config, interpolParsedCode, interpolIndenter);
			markTokenText.run();
			markWhitespace.run();

			var outputLineEnds:String = MarkLineEnds.outputLineSeparator(config.lineEnds, interpolParsedCode);
			var lines:CodeLines = new CodeLines(interpolParsedCode, interpolIndenter);
			var formatted:String = lines.print(outputLineEnds);
			return formatted.trim();
		} catch (e:Any) {
			// ignore any errors
		}
		return fragment;
	}

	function makeTokens(fragment:ByteData, name:String):Array<Token> {
		var tokens:Array<Token> = [];
		try {
			var lexer = new HaxeLexer(fragment, name);
			var t:Token = lexer.token(haxeparser.HaxeLexer.tok);

			while (t.tok != Eof) {
				tokens.push(t);
				t = lexer.token(haxeparser.HaxeLexer.tok);
			}
		} catch (e:Any) {
			throw 'failed to make tokens $e';
		}
		return tokens;
	}

	public function printEregToken(token:TokenTree):String {
		return parsedCode.getString(token.pos.min, token.pos.max);
	}

	public function printComment(text:String, token:TokenTree):String {
		var lines:Array<String> = text.split(parsedCode.lineSeparator);
		var indent:Int = indenter.calcIndent(token);

		var startsWithStar:Bool = lines.length >= 3;
		for (index in 1...lines.length - 1) {
			if (!~/^\s*\*(\s|$)/.match(lines[index])) {
				startsWithStar = false;
				break;
			}
		}

		var linesNew:Array<String> = [];
		for (line in lines) {
			linesNew.push(convertLeadingIndent(line));
		}
		lines = removeCommentPrefix(linesNew);

		var outputLineEnds:String = MarkLineEnds.outputLineSeparator(config.lineEnds, parsedCode);
		text = "/*" + lines[0];
		for (index in 1...lines.length) {
			text += outputLineEnds;
			var line:String = lines[index];
			var lineIndent:Int = indent;
			var lastLine:Bool = index == lines.length - 1;
			if (!lastLine) {
				lineIndent++;
			}
			if (startsWithStar) {
				lineIndent = indent;
			}
			if (!lastLine && line.length <= 0) {
				lineIndent = 0;
			}
			if (!lastLine && startsWithStar) {
				line = " " + line;
			}
			if (lastLine) {
				if (~/^\s*\*\s*[^\s\*]/.match(line)) {
					line = " " + line;
				}
				var leadingWS:EReg = ~/^\s*}/;
				if (leadingWS.match(line)) {
					line = line.trim();
				} else {
					if (~/^\s*[^*\s]/.match(line)) {
						lineIndent = indent + 1;
					}
					line = line.rtrim();
					if (!line.endsWith("*")) {
						line += " ";
					}
				}
				if (~/^\s*$/.match(line)) {
					line = " ";
				}
			}
			text += indenter.makeIndentString(lineIndent) + line;
		}
		return text + "*/";
	}

	function removeCommentPrefix(lines:Array<String>):Array<String> {
		var prefixReg:EReg = ~/^(\s*)/;
		var prefix:Null<String> = null;
		var linesNew:Array<String> = [];
		var endIndex:Int = lines.length - 1;
		var lastLine:String = lines[lines.length - 1];
		if (!~/^\s*(\**$|\})/.match(lastLine)) {
			endIndex = lines.length;
		}
		for (index in 1...endIndex) {
			var line:String = lines[index];
			prefixReg.match(line);
			var linePrefix:String = prefixReg.matched(1);
			if (linePrefix.length <= 0) {
				continue;
			}
			if ((prefix == null) || (prefix.length > linePrefix.length)) {
				prefix = linePrefix;
			}
		}
		if (prefix != null) {
			linesNew = [];
			var startPrefix:String = prefix + " *";
			for (line in lines) {
				if (line.startsWith(startPrefix)) {
					line = line.substr(startPrefix.length - 1);
				}
				if (line.startsWith(prefix)) {
					line = line.substr(prefix.length);
				}
				linesNew.push(line);
			}
			lines = linesNew;
		}
		var lastLine:String = lines[lines.length - 1];
		if (~/^\s*\*\**$/.match(lastLine)) {
			lines[lines.length - 1] = lastLine.ltrim();
		}
		return lines;
	}

	function convertLeadingIndent(line:String):String {
		var spaceIndent:String = "".lpad(" ", config.indentation.tabWidth);
		var oneIndent:String = config.indentation.character;
		var whitespaceReg:EReg = ~/^\s+/;
		if (!whitespaceReg.match(line)) {
			return line;
		}
		var match:String = whitespaceReg.matched(0);
		if (config.indentation.character == "\t") {
			var newPrefix:String = match.replace(spaceIndent, oneIndent);
			line = newPrefix + line.substr(match.length);
		} else {
			var newPrefix:String = match.replace("\t", oneIndent);
			line = newPrefix + line.substr(match.length);
		}
		return line;
	}

	public function printCommentLine(text:String):String {
		if (~/^[\/\*\-\s]+/.match(text)) {
			return "//" + text.rtrim();
		}
		if (config.whitespace.addLineCommentSpace) {
			return "// " + text.trim();
		}
		return "//" + text.trim();
	}
}
