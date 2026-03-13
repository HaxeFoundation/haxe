package haxeparser;

import haxeparser.Data;
import haxe.macro.Expr;
import hxparse.Lexer;

enum LexerErrorMsg {
	UnterminatedString;
	UnterminatedRegExp;
	UnclosedComment;
	UnterminatedEscapeSequence;
	InvalidEscapeSequence(c:String);
	UnknownEscapeSequence(c:String);
	UnclosedCode;
}

class LexerError {
	public var msg:LexerErrorMsg;
	public var pos:Position;
	public function new(msg, pos) {
		this.msg = msg;
		this.pos = pos;
	}
}

class HaxeLexer extends Lexer implements hxparse.RuleBuilder {

	static function mkPos(p:hxparse.Position) {
		return {
			file: p.psource,
			min: p.pmin,
			max: p.pmax
		};
	}

	static function mk(lexer:Lexer, td) {
		return new Token(td, mkPos(lexer.curPos()));
	}

	// @:mapping generates a map with lowercase enum constructor names as keys
	// and the constructor itself as value
	static var keywords = @:mapping(3) Data.Keyword;

	static var buf = new StringBuf();

	static var ident = "_*[a-z][a-zA-Z0-9_]*|_+|_+[0-9][_a-zA-Z0-9]*";
	static var idtype = "_*[A-Z][a-zA-Z0-9_]*";

	// @:rule wraps the expression to the right of => with function(lexer) return
	public static var tok = @:rule [
		"" => mk(lexer, Eof),
		"[\r\n\t ]+" => {
			#if keep_whitespace
			var space = lexer.current;
			var token:Token = lexer.token(tok);
			token.space = space;
			token;
			#else
			lexer.token(tok);
			#end
		},
		"0x[0-9a-fA-F]+" => mk(lexer, Const(CInt(lexer.current))),
		"[0-9]+" => mk(lexer, Const(CInt(lexer.current))),
		"[0-9]+\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		"\\.[0-9]+" => mk(lexer, Const(CFloat(lexer.current))),
		"[0-9]+[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		"[0-9]+\\.[0-9]*[eE][\\+\\-]?[0-9]+" => mk(lexer,Const(CFloat(lexer.current))),
		"[0-9]+\\.\\.\\." => mk(lexer,IntInterval(lexer.current.substr(0,-3))),
		"//[^\n\r]*" => mk(lexer, CommentLine(lexer.current.substr(2))),
		"+\\+" => mk(lexer,Unop(OpIncrement)),
		"--" => mk(lexer,Unop(OpDecrement)),
		"~" => mk(lexer,Unop(OpNegBits)),
		"%=" => mk(lexer,Binop(OpAssignOp(OpMod))),
		"&=" => mk(lexer,Binop(OpAssignOp(OpAnd))),
		"|=" => mk(lexer,Binop(OpAssignOp(OpOr))),
		"^=" => mk(lexer,Binop(OpAssignOp(OpXor))),
		"+=" => mk(lexer,Binop(OpAssignOp(OpAdd))),
		"-=" => mk(lexer,Binop(OpAssignOp(OpSub))),
		"*=" => mk(lexer,Binop(OpAssignOp(OpMult))),
		"/=" => mk(lexer,Binop(OpAssignOp(OpDiv))),
		"<<=" => mk(lexer,Binop(OpAssignOp(OpShl))),
		"==" => mk(lexer,Binop(OpEq)),
		"!=" => mk(lexer,Binop(OpNotEq)),
		"<=" => mk(lexer,Binop(OpLte)),
		"&&" => mk(lexer,Binop(OpBoolAnd)),
		"|\\|" => mk(lexer,Binop(OpBoolOr)),
		"<<" => mk(lexer,Binop(OpShl)),
		"->" => mk(lexer,Arrow),
		"\\.\\.\\." => mk(lexer,Binop(OpInterval)),
		"=>" => mk(lexer,Binop(OpArrow)),
		"!" => mk(lexer,Unop(OpNot)),
		"<" => mk(lexer,Binop(OpLt)),
		">" => mk(lexer,Binop(OpGt)),
		";" => mk(lexer, Semicolon),
		":" => mk(lexer, DblDot),
		"," => mk(lexer, Comma),
		"\\." => mk(lexer, Dot),
		"%" => mk(lexer,Binop(OpMod)),
		"&" => mk(lexer,Binop(OpAnd)),
		"|" => mk(lexer,Binop(OpOr)),
		"^" => mk(lexer,Binop(OpXor)),
		"+" => mk(lexer,Binop(OpAdd)),
		"*" => mk(lexer,Binop(OpMult)),
		"/" => mk(lexer,Binop(OpDiv)),
		"-" => mk(lexer,Binop(OpSub)),
		"=" => mk(lexer,Binop(OpAssign)),
		"[" => mk(lexer, BkOpen),
		"]" => mk(lexer, BkClose),
		"{" => mk(lexer, BrOpen),
		"}" => mk(lexer, BrClose),
		"\\(" => mk(lexer, POpen),
		"\\)" => mk(lexer, PClose),
		"?" => mk(lexer, Question),
		"@" => mk(lexer, At),
		'"' => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(string) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedString, mkPos(pmin));
			var token = mk(lexer, Const(CString(unescape(buf.toString(), mkPos(pmin)))));
			token.pos.min = pmin.pmin; token;
		},
		"'" => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(string2) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedString, mkPos(pmin));
			var token = mk(lexer, Const(CString(unescape(buf.toString(), mkPos(pmin)))));
			token.pos.min = pmin.pmin; token;
		},
		'~/' => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var info = try lexer.token(regexp) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedRegExp, mkPos(pmin));
			var token = mk(lexer, Const(CRegexp(buf.toString(), info.opt)));
			token.pos.min = pmin.pmin; token;
		},
		'/\\*' => {
			buf = new StringBuf();
			var pmin = lexer.curPos();
			var pmax = try lexer.token(comment) catch (e:haxe.io.Eof) throw new LexerError(UnclosedComment, mkPos(pmin));
			var token = mk(lexer, Comment(buf.toString()));
			token.pos.min = pmin.pmin; token;
		},
		"#" + ident => mk(lexer, Sharp(lexer.current.substr(1))),
		"$[_a-zA-Z0-9]*" => mk(lexer, Dollar(lexer.current.substr(1))),
		ident => {
			var kwd = keywords.get(lexer.current);
			if (kwd != null)
				mk(lexer, Kwd(kwd));
			else
				mk(lexer, Const(CIdent(lexer.current)));
		},
		idtype => mk(lexer, Const(CIdent(lexer.current))),
	];

	public static var string = @:rule [
		"\\\\\\\\" => {
			buf.add("\\\\");
			lexer.token(string);
		},
		"\\\\" => {
			buf.add("\\");
			lexer.token(string);
		},
		"\\\\\"" => {
			buf.add('"');
			lexer.token(string);
		},
		'"' => lexer.curPos().pmax,
		"[^\\\\\"]+" => {
			buf.add(lexer.current);
			lexer.token(string);
		}
	];

	public static var string2 = @:rule [
		"\\\\\\\\" => {
			buf.add("\\\\");
			lexer.token(string2);
		},
		"\\\\" => {
			buf.add("\\");
			lexer.token(string2);
		},
		'\\\\\'' => {
			buf.add("'");
			lexer.token(string2);
		},
		"'" => lexer.curPos().pmax,
		"($$)|(\\$)|$" => {
			buf.add("$");
			lexer.token(string2);
		},
		"${" => {
			var pmin = lexer.curPos();
			buf.add(lexer.current);
			try lexer.token(codeString) catch(e:haxe.io.Eof) throw new LexerError(UnclosedCode, mkPos(pmin));
			lexer.token(string2);
		},
		"[^$\\\\']+" => {
			buf.add(lexer.current);
			lexer.token(string2);
		}
	];

	public static var codeString = @:rule [
		"{|/" => {
			buf.add(lexer.current);
			lexer.token(codeString);
		},
		"}" => {
			buf.add(lexer.current);
		},
		'"' => {
			buf.addChar('"'.code);
			var pmin = lexer.curPos();
			try lexer.token(string) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedString, mkPos(pmin));
			buf.addChar('"'.code);
			lexer.token(codeString);
		},
		"'" => {
			buf.addChar("'".code);
			var pmin = lexer.curPos();
			try lexer.token(string2) catch (e:haxe.io.Eof) throw new LexerError(UnterminatedString, mkPos(pmin));
			buf.addChar("'".code);
			lexer.token(codeString);
		},
		'/\\*' => {
			var pmin = lexer.curPos();
			try lexer.token(comment) catch (e:haxe.io.Eof) throw new LexerError(UnclosedComment, mkPos(pmin));
			lexer.token(codeString);
		},
		"//[^\n\r]*" => {
			buf.add(lexer.current);
			lexer.token(codeString);
		},
		"[^/\"'{}\n\r]+" => {
			buf.add(lexer.current);
			lexer.token(codeString);
		}
	];

	public static var comment = @:rule [
		"*/" => lexer.curPos().pmax,
		"*" => {
			buf.add("*");
			lexer.token(comment);
		},
		"[^\\*]+" => {
			buf.add(lexer.current);
			lexer.token(comment);
		}
	];

	public static var regexp = @:rule [
		"\\\\/" => {
			buf.add("/");
			lexer.token(regexp);
		},
		"\\\\r" => {
			buf.add("\r");
			lexer.token(regexp);
		},
		"\\\\n" => {
			buf.add("\n");
			lexer.token(regexp);
		},
		"\\\\t" => {
			buf.add("\t");
			lexer.token(regexp);
		},
		"\\\\[\\\\$\\.*+\\^|{}\\[\\]()?\\-0-9]" => {
			buf.add(lexer.current);
			lexer.token(regexp);
		},
		"\\\\[wWbBsSdDx]" => {
			buf.add(lexer.current);
			lexer.token(regexp);
		},
		"/" => {
			lexer.token(regexp_options);
		},
		"[^\\\\/\r\n]+" => {
			buf.add(lexer.current);
			lexer.token(regexp);
		}
	];

	public static var regexp_options = @:rule [
		"[gimsu]*" => {
			{ pmax:lexer.curPos().pmax, opt:lexer.current };
		}
	];

	static inline function unescapePos(pos:Position, index:Int, length:Int) {
		return {
			file: pos.file,
			min: pos.min + index,
			max: pos.min + index + length
		}
	}

	static function unescape(s:String, pos:Position) {
		var b = new StringBuf();
		var i = 0;
		var esc = false;
		while (true) {
			if (s.length == i) {
				break;
			}
			var c = s.charCodeAt(i);
			if (esc) {
				var iNext = i + 1;
				switch (c) {
					case 'n'.code: b.add("\n");
					case 'r'.code: b.add("\r");
					case 't'.code: b.add("\t");
					case '"'.code | '\''.code | '\\'.code: b.addChar(c);
					case _ >= '0'.code && _ <= '3'.code => true:
						iNext += 2;
					case 'x'.code:
						var chars = s.substr(i + 1, 2);
						if (!(~/^[0-9a-fA-F]{2}$/.match(chars))) throw new LexerError(InvalidEscapeSequence("\\x"+chars), unescapePos(pos, i, 1 + 2));
						var c = Std.parseInt("0x" + chars);
						b.addChar(c);
						iNext += 2;
					case 'u'.code:
						var c:Int;
						if (s.charAt(i + 1) == "{") {
							var endIndex = s.indexOf("}", i + 3);
							if (endIndex == -1) throw new LexerError(UnterminatedEscapeSequence, unescapePos(pos, i, 2));
							var l = endIndex - (i + 2);
							var chars = s.substr(i + 2, l);
							if (!(~/^[0-9a-fA-F]+$/.match(chars))) throw new LexerError(InvalidEscapeSequence("\\u{"+chars+"}"), unescapePos(pos, i, 1 + 2 + l));
							c = Std.parseInt("0x" + chars);
							if (c > 0x10FFFF) throw new LexerError(InvalidEscapeSequence("\\u{"+chars+"}"), unescapePos(pos, i, 1 + 2 + l));
							iNext += 2 + l;
						} else {
							var chars = s.substr(i + 1, 4);
							if (!(~/^[0-9a-fA-F]{4}$/.match(chars))) throw new LexerError(InvalidEscapeSequence("\\u"+chars), unescapePos(pos, i, 1 + 4));
							c = Std.parseInt("0x" + chars);
							iNext += 4;
						}
						b.addChar(c);
					case c:
						throw new LexerError(UnknownEscapeSequence("\\"+String.fromCharCode(c)), unescapePos(pos, i, 1));
				}
				esc = false;
				i = iNext;
			} else switch (c) {
				case '\\'.code:
					++i;
					esc = true;
				case _:
					b.addChar(c);
					++i;
			}

		}
		return b.toString();
	}
}