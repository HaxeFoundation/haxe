package formatter.codedata;

import haxeparser.HaxeLexer;
import hxparse.ParserError;
import tokentree.TokenTreeBuilder;

class ParsedCode {
	static inline var BAD_OFFSET:String = "Bad offset";

	var file(default, null):ParseFile;

	public var tokens(default, null):Array<Token>;
	public var linesIdx(default, null):Array<LineIds>;
	public var lines(default, null):Array<String>;
	public var lineSeparator(default, null):String;
	public var root(default, null):TokenTree;
	public var tokenList(default, null):TokenList;
	public var emptyLines(default, null):Array<Int>;

	public function new(inputData:FormatterInputData) {
		file = {
			name: inputData.fileName,
			content: inputData.content
		};
		try {
			removeBOM();
			if (inputData.lineSeparator == null) {
				detectLineSeparator();
			} else {
				lineSeparator = inputData.lineSeparator;
			}
			makeLines();
			makePosIndices();
			if (inputData.tokenList == null) {
				makeTokens();
				getTokenTree(inputData.entryPoint);
			} else {
				tokens = inputData.tokenList;
				root = inputData.tokenTree;
				makeTokenList();
			}
			// sanity check: tokens vs tokenlist
			checkTokens();
		} catch (e:Any) {
			throw 'failed to create parser context: $e';
		}
	}

	function checkTokens() {
		if (tokens.length != tokenList.tokens.length) {
			throw "token count mismatch";
		}
		var skipCount:Int = 0;
		for (index in 0...tokens.length) {
			var info:TokenInfo = tokenList.tokens[index];
			if (info == null) {
				if (skipCount <= 0) {
					throw 'missing token "${tokens[index]}" [$index] detected!';
				}
				skipCount--;
				continue;
			}
			var token:TokenTree = info.token;
			switch (token.tok) {
				case Binop(OpAssignOp(OpUShr)):
					skipCount = 3;
				case Binop(OpUShr):
					skipCount = 2;
				case Binop(OpAssignOp(OpShr)):
					skipCount = 2;
				case Binop(OpShr):
					skipCount = 1;
				case Binop(OpGte):
					skipCount = 1;
				case Const(CMarkup(_)):
					elimateMulitlineEmptyLines(token.pos);
					var skipIndex = index + 1;
					while (true) {
						if (tokens[skipIndex].pos.min >= token.pos.max) {
							break;
						}
						skipCount++;
						skipIndex++;
					}
				case Const(CInt(v)):
					if (v.startsWith("-")) {
						skipCount = 1;
					}
				case Const(CFloat(v)):
					if (v.startsWith("-")) {
						skipCount = 1;
					}
				case Comment(_) | Const(CString(_)):
					elimateMulitlineEmptyLines(token.pos);
					skipCount = 0;
				default:
					skipCount = 0;
			}
		}
	}

	function elimateMulitlineEmptyLines(pos:Position) {
		var start:LinePos = getLinePos(pos.min);
		var end:LinePos = getLinePos(pos.max);
		for (i in start.line...end.line) {
			emptyLines.remove(i);
		}
	}

	public function getTokenTree(entryPoint:Null<TokenTreeEntryPoint> = null):TokenTree {
		if (tokens == null) {
			return null;
		}
		if (entryPoint == null) {
			entryPoint = TypeLevel;
		}
		if (root == null) {
			root = TokenTreeBuilder.buildTokenTree(tokens, ByteData.ofBytes(file.content), entryPoint);
			makeTokenList();
		}
		return root;
	}

	function removeBOM() {
		if ((file.content == null) || (file.content.length <= 2)) {
			return;
		}
		if ((file.content.get(0) == 0xEF) && (file.content.get(1) == 0xBB) && (file.content.get(2) == 0xBF)) {
			var withBOM:Bytes = file.content;
			file.content = withBOM.sub(3, file.content.length - 3);
		}
	}

	function makeTokenList() {
		tokenList = new TokenList();
		tokenList.buildList(root);
	}

	function makePosIndices() {
		var code:Bytes = file.content;
		linesIdx = [];

		var last = 0;
		var left = false;

		var skip0A:Bool = false;
		for (i in 0...code.length) {
			var charCode:Int = code.get(i);
			if (skip0A && (charCode == 0x0A)) {
				skip0A = false;
				continue;
			}
			if ((charCode == 0x0A) || (charCode == 0x0D)) {
				if ((charCode == 0x0D) && ((i + 1) < code.length) && (code.get(i + 1) == 0x0A)) {
					skip0A = true;
					linesIdx.push({l: last, r: i + 1});
					last = i + 2;
				} else {
					linesIdx.push({l: last, r: i});
					last = i + 1;
				}
			}
			left = true;
		}
		if (left) {
			linesIdx.push({l: last, r: code.length});
		}
	}

	public function getLinePos(off:Int):LinePos {
		var lowerBound:Int = 0;
		var upperBound:Int = linesIdx.length - 1;
		if (linesIdx.length <= 0) {
			throw BAD_OFFSET;
		}

		if (off < 0) {
			throw BAD_OFFSET;
		}

		if (off > linesIdx[upperBound].r) {
			throw BAD_OFFSET;
		}

		while (true) {
			if (lowerBound > upperBound) {
				throw BAD_OFFSET;
			}

			var center:Int = lowerBound + Math.floor((upperBound - lowerBound) / 2);
			var matchLeft:Bool = linesIdx[center].l <= off;
			var matchRight:Bool = linesIdx[center].r >= off;
			if (matchLeft && matchRight) {
				return {
					line: center,
					ofs: off - linesIdx[center].l
				};
			}
			if (matchLeft) {
				lowerBound = center + 1;
				continue;
			}
			if (matchRight) {
				upperBound = center - 1;
				continue;
			}
		}
		throw BAD_OFFSET;
	}

	public function getString(off:Int, off2:Int):String {
		var code:Bytes = file.content;
		var len:Int = off2 - off;
		if ((off >= code.length) || (off + len > code.length)) {
			return "";
		}
		return code.sub(off, off2 - off).toString();
	}

	public function isOriginalNewlineBefore(token:TokenTree):Bool {
		if (token == null) {
			return false;
		}
		var prev:TokenInfo = tokenList.getPreviousToken(token);
		if (prev == null) {
			return false;
		}
		return !isOriginalSameLine(prev.token, token);
	}

	public function isOriginalSameLine(first:TokenTree, second:TokenTree):Bool {
		var startLine:Int = getLinePos(first.pos.max).line;
		var endLine:Int = getLinePos(second.pos.min).line;
		return (startLine == endLine);
	}

	public function linesBetweenOriginal(first:TokenTree, second:TokenTree):Int {
		var startLine:Int = getLinePos(first.pos.min).line;
		var endLine:Int = getLinePos(second.pos.min).line;
		return (endLine - startLine);
	}

	function detectLineSeparator() {
		var codeBytes:Bytes = file.content;
		var code:String = codeBytes.toString();

		for (i in 0...code.length) {
			var char = code.charAt(i);
			if ((char == "\r") || (char == "\n")) {
				lineSeparator = char;
				if ((char == "\r") && (i + 1 < code.length)) {
					char = code.charAt(i + 1);
					if (char == "\n") {
						lineSeparator += char;
					}
				}
				return;
			}
		}

		// default
		lineSeparator = "\n";
	}

	function makeLines() {
		var code:Bytes = file.content;
		var textCode:String = code.toString();
		lines = textCode.split(lineSeparator);

		emptyLines = [];
		for (index in 0...lines.length) {
			var line:String = lines[index];
			if (line.startsWith("<<<<<<<")) {
				throw 'not formatting "${file.name}" - file contains a merge conflict';
			}
			if (~/^\s*$/.match(line)) {
				emptyLines.push(index);
			}
		}
	}

	function makeTokens() {
		try {
			tokens = [];
			root = null;
			var lexer = new HaxeLexer(ByteData.ofBytes(file.content), file.name);
			var t:Token = lexer.token(haxeparser.HaxeLexer.tok);

			while (t.tok != Eof) {
				tokens.push(t);
				t = lexer.token(haxeparser.HaxeLexer.tok);
			}
		} catch (e:ParserError) {
			throw 'failed to make tokens - ParserError: $e (${e.pos})';
		} catch (e:LexerError) {
			throw 'failed to make tokens - LexerError: ${e.msg} (${e.pos})';
		} catch (e:Any) {
			throw 'failed to make tokens $e';
		}
	}
}

typedef LinePos = {
	var line:Int;
	var ofs:Int;
}

typedef LineIds = {
	var l:Int;
	var r:Int;
}
