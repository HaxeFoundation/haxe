/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

package haxe.format;

/**
	An implementation of JSON parser in Haxe.

	This class is used by `haxe.Json` when native JSON implementation
	is not available.

	@see https://haxe.org/manual/std-Json-parsing.html
**/
class JsonParser {
	/**
		Parses given JSON-encoded `str` and returns the resulting object.

		JSON objects are parsed into anonymous structures and JSON arrays
		are parsed into `Array<Dynamic>`.

		If given `str` is not valid JSON, an exception will be thrown.

		If `str` is null, the result is unspecified.
	**/
	static public inline function parse(str:String):Dynamic {
		return new JsonParser(str).doParse();
	}

	var str:String;
	var pos:Int;

	function new(str:String) {
		this.str = str;
		this.pos = 0;
	}

	function doParse():Dynamic {
		var result = parseRec();
		var c;
		while (!StringTools.isEof(c = nextChar())) {
			switch (c) {
				case ' '.code, '\r'.code, '\n'.code, '\t'.code:
				// allow trailing whitespace
				default:
					invalidChar();
			}
		}
		return result;
	}

	function parseRec():Dynamic {
		while (true) {
			var c = nextChar();
			switch (c) {
				case ' '.code, '\r'.code, '\n'.code, '\t'.code:
				// loop
				case '{'.code:
					var obj = {}, field = null, comma:Null<Bool> = null;
					while (true) {
						var c = nextChar();
						switch (c) {
							case ' '.code, '\r'.code, '\n'.code, '\t'.code:
							// loop
							case '}'.code:
								if (field != null || comma == false)
									invalidChar();
								return obj;
							case ':'.code:
								if (field == null)
									invalidChar();
								Reflect.setField(obj, field, parseRec());
								field = null;
								comma = true;
							case ','.code:
								if (comma) comma = false else invalidChar();
							case '"'.code:
								if (field != null || comma) invalidChar();
								field = parseString();
							default:
								invalidChar();
						}
					}
				case '['.code:
					var arr = [], comma:Null<Bool> = null;
					while (true) {
						var c = nextChar();
						switch (c) {
							case ' '.code, '\r'.code, '\n'.code, '\t'.code:
							// loop
							case ']'.code:
								if (comma == false) invalidChar();
								return arr;
							case ','.code:
								if (comma) comma = false else invalidChar();
							default:
								if (comma) invalidChar();
								pos--;
								arr.push(parseRec());
								comma = true;
						}
					}
				case 't'.code:
					var save = pos;
					if (nextChar() != 'r'.code || nextChar() != 'u'.code || nextChar() != 'e'.code) {
						pos = save;
						invalidChar();
					}
					return true;
				case 'f'.code:
					var save = pos;
					if (nextChar() != 'a'.code || nextChar() != 'l'.code || nextChar() != 's'.code || nextChar() != 'e'.code) {
						pos = save;
						invalidChar();
					}
					return false;
				case 'n'.code:
					var save = pos;
					if (nextChar() != 'u'.code || nextChar() != 'l'.code || nextChar() != 'l'.code) {
						pos = save;
						invalidChar();
					}
					return null;
				case '"'.code:
					return parseString();
				case '0'.code, '1'.code, '2'.code, '3'.code, '4'.code, '5'.code, '6'.code, '7'.code, '8'.code, '9'.code, '-'.code:
					return parseNumber(c);
				default:
					invalidChar();
			}
		}
	}

	function parseString() {
		var start = pos;
		var buf:StringBuf = null;
		#if target.unicode
		var prev = -1;
		inline function cancelSurrogate() {
			// invalid high surrogate (not followed by low surrogate)
			buf.addChar(0xFFFD);
			prev = -1;
		}
		#end
		while (true) {
			var c = nextChar();
			if (c == '"'.code)
				break;
			if (c == '\\'.code) {
				if (buf == null) {
					buf = new StringBuf();
				}
				buf.addSub(str, start, pos - start - 1);
				c = nextChar();
				#if target.unicode
				if (c != "u".code && prev != -1)
					cancelSurrogate();
				#end
				switch (c) {
					case "r".code:
						buf.addChar("\r".code);
					case "n".code:
						buf.addChar("\n".code);
					case "t".code:
						buf.addChar("\t".code);
					case "b".code:
						buf.addChar(8);
					case "f".code:
						buf.addChar(12);
					case "/".code, '\\'.code, '"'.code:
						buf.addChar(c);
					case 'u'.code:
						var uc:Int = Std.parseInt("0x" + str.substr(pos, 4));
						pos += 4;
						#if !target.unicode
						if (uc <= 0x7F)
							buf.addChar(uc);
						else if (uc <= 0x7FF) {
							buf.addChar(0xC0 | (uc >> 6));
							buf.addChar(0x80 | (uc & 63));
						} else if (uc <= 0xFFFF) {
							buf.addChar(0xE0 | (uc >> 12));
							buf.addChar(0x80 | ((uc >> 6) & 63));
							buf.addChar(0x80 | (uc & 63));
						} else {
							buf.addChar(0xF0 | (uc >> 18));
							buf.addChar(0x80 | ((uc >> 12) & 63));
							buf.addChar(0x80 | ((uc >> 6) & 63));
							buf.addChar(0x80 | (uc & 63));
						}
						#else
						if (prev != -1) {
							if (uc < 0xDC00 || uc > 0xDFFF)
								cancelSurrogate();
							else {
								buf.addChar(((prev - 0xD800) << 10) + (uc - 0xDC00) + 0x10000);
								prev = -1;
							}
						} else if (uc >= 0xD800 && uc <= 0xDBFF)
							prev = uc;
						else
							buf.addChar(uc);
						#end
					default:
						throw "Invalid escape sequence \\" + String.fromCharCode(c) + " at position " + (pos - 1);
				}
				start = pos;
			}
			#if !(target.unicode)
			// ensure utf8 chars are not cut
			else if (c >= 0x80) {
				pos++;
				if (c >= 0xFC)
					pos += 4;
				else if (c >= 0xF8)
					pos += 3;
				else if (c >= 0xF0)
					pos += 2;
				else if (c >= 0xE0)
					pos++;
			}
			#end
		else if (StringTools.isEof(c))
			throw "Unclosed string";
		}
		#if target.unicode
		if (prev != -1)
			cancelSurrogate();
		#end
		if (buf == null) {
			return str.substr(start, pos - start - 1);
		} else {
			buf.addSub(str, start, pos - start - 1);
			return buf.toString();
		}
	}

	inline function parseNumber(c:Int):Dynamic {
		var start = pos - 1;
		var minus = c == '-'.code, digit = !minus, zero = c == '0'.code;
		var point = false, e = false, pm = false, end = false;
		while (true) {
			c = nextChar();
			switch (c) {
				case '0'.code:
					if (zero && !point)
						invalidNumber(start);
					if (minus) {
						minus = false;
						zero = true;
					}
					digit = true;
				case '1'.code, '2'.code, '3'.code, '4'.code, '5'.code, '6'.code, '7'.code, '8'.code, '9'.code:
					if (zero && !point)
						invalidNumber(start);
					if (minus)
						minus = false;
					digit = true;
					zero = false;
				case '.'.code:
					if (minus || point || e)
						invalidNumber(start);
					digit = false;
					point = true;
				case 'e'.code, 'E'.code:
					if (minus || zero || e)
						invalidNumber(start);
					digit = false;
					e = true;
				case '+'.code, '-'.code:
					if (!e || pm)
						invalidNumber(start);
					digit = false;
					pm = true;
				default:
					if (!digit)
						invalidNumber(start);
					pos--;
					end = true;
			}
			if (end)
				break;
		}

		var f = Std.parseFloat(str.substr(start, pos - start));
		if(point) {
			return f;
		} else {
			var i = Std.int(f);
			return if (i == f) i else f;
		}
	}

	inline function nextChar() {
		return StringTools.fastCodeAt(str, pos++);
	}

	function invalidChar() {
		pos--; // rewind
		throw "Invalid char " + StringTools.fastCodeAt(str, pos) + " at position " + pos;
	}

	function invalidNumber(start:Int) {
		throw "Invalid number at position " + start + ": " + str.substr(start, pos - start);
	}
}
