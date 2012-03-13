/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package haxe;

/**
	Crossplatform JSON API : it will automatically use the optimized native API if available.
	Use -D haxeJSON to force usage of the haXe implementation even if a native API is found : this will provide
	extra encoding features such as enums (replaced by their index), Hashs and Iterable.
**/
#if (flash11 && !haxeJSON)
@:native('JSON') extern
#end
class Json {

#if (haxeJSON || !flash11)
	var buf : StringBuf;
	var str : String;
	var pos : Int;
	var reg_float : EReg;

	function new() {
	}

	function toString(v:Dynamic) {
		buf = new StringBuf();
		toStringRec(v);
		return buf.toString();
	}

	function objString( v : Dynamic ) {
		var first = true;
		buf.add('{');
		for( f in Reflect.fields(v) ) {
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( first ) first = false else buf.add(',');
			quote(f);
			buf.add(':');
			toStringRec(value);
		}
		buf.add('}');
	}

	function toStringRec(v:Dynamic) {
		switch( Type.typeof(v) ) {
		case TUnknown:
			buf.add('"???"');
		case TObject:
			objString(v);
		case TInt,TFloat:
			buf.add(v);
		case TFunction:
			buf.add('"<fun>"');
		case TClass(c):
			if( c == String )
				quote(v);
			else if( c == Array ) {
				var v : Array<Dynamic> = v;
				buf.add('[');
				var len = v.length;
				if( len > 0 ) {
					toStringRec(v[0]);
					var i = 1;
					while( i < len ) {
						buf.add(',');
						toStringRec(v[i++]);
					}
				}
				buf.add(']');
			} else if( c == Hash ) {
				var v : Hash<Dynamic> = v;
				var o = {};
				for( k in v.keys() )
					Reflect.setField(o,k,v.get(k));
				objString(o);
			} else if( v.iterator != null ) {
				var a = [];
				var it : Iterator<Dynamic> = v.iterator();
				for( v in it )
					a.push(v);
				toStringRec(a);
			} else
				objString(v);
		case TEnum(e):
			buf.add(Type.enumIndex(v));
		case TBool:
			buf.add(v ? 'true' : 'false');
		case TNull:
			buf.add('null');
		}
	}

	function quote( s : String ) {
		#if (neko || php || cpp)
		if( s.length != haxe.Utf8.length(s) ) {
			quoteUtf8(s);
			return;
		}
		#end
		buf.add('"');
		var i = 0;
		while( true ) {
			var c = StringTools.fastCodeAt(s,i++);
			if( StringTools.isEOF(c) ) break;
			switch( c ) {
			case '/'.code: buf.add('\\/');
			case '"'.code: buf.add('\\"');
			case '\\'.code: buf.add('\\\\');
			case '\n'.code: buf.add('\\n');
			case '\r'.code: buf.add('\\r');
			case '\t'.code: buf.add('\\t');
			case 8: buf.add('\\b');
			case 12: buf.add('\\f');
			default: buf.addChar(c);
			}
		}
		buf.add('"');
	}

	#if (neko || php || cpp)
	function quoteUtf8( s : String ) {
		var u = new haxe.Utf8();
		haxe.Utf8.iter(s,function(c) {
			switch( c ) {
			case '/'.code, '\\'.code, '"'.code: u.addChar('\\'.code); u.addChar(c);
			case '\n'.code: u.addChar('\\'.code); u.addChar('n'.code);
			case '\r'.code: u.addChar('\\'.code); u.addChar('r'.code);
			case '\t'.code: u.addChar('\\'.code); u.addChar('t'.code);
			case 8: u.addChar('\\'.code); u.addChar('b'.code);
			case 12: u.addChar('\\'.code); u.addChar('f'.code);
			default: u.addChar(c);
			}
		});
		buf.add("'");
		buf.add(u.toString());
		buf.add("'");
	}
	#end

	function doParse( str : String ) {
		reg_float = ~/^-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?/;
		this.str = str;
		this.pos = 0;
		return parseRec();
	}

	function invalidChar() {
		pos--; // rewind
		throw "Invalid char "+StringTools.fastCodeAt(str,pos)+" at position "+pos;
	}

	inline function nextChar() {
		return StringTools.fastCodeAt(str,pos++);
	}

	function parseRec() : Dynamic {
		while( true ) {
			var c = nextChar();
			switch( c ) {
			case ' '.code, '\r'.code, '\n'.code, '\t'.code:
				// loop
			case '{'.code:
				var obj = {}, field = null, comma : Null<Bool> = null;
				while( true ) {
					var c = nextChar();
					switch( c ) {
					case ' '.code, '\r'.code, '\n'.code, '\t'.code:
						// loop
					case '}'.code:
						if( field != null || comma == false )
							invalidChar();
						return obj;
					case ':'.code:
						if( field == null )
							invalidChar();
						Reflect.setField(obj,field,parseRec());
						field = null;
						comma = true;
					case ','.code:
						if( comma ) comma = false else invalidChar();
					case '"'.code:
						if( comma ) invalidChar();
						field = parseString();
					default:
						invalidChar();
					}
				}
			case '['.code:
				var arr = [], comma : Null<Bool> = null;
				while( true ) {
					var c = nextChar();
					switch( c ) {
					case ' '.code, '\r'.code, '\n'.code, '\t'.code:
						// loop
					case ']'.code:
						if( comma == false ) invalidChar();
						return arr;
					case ','.code:
						if( comma ) comma = false else invalidChar();
					default:
						if( comma ) invalidChar();
						pos--;
						arr.push(parseRec());
						comma = true;
					}
				}
			case 't'.code:
				var save = pos;
				if( nextChar() != 'r'.code || nextChar() != 'u'.code || nextChar() != 'e'.code ) {
					pos = save;
					invalidChar();
				}
				return true;
			case 'f'.code:
				var save = pos;
				if( nextChar() != 'a'.code || nextChar() != 'l'.code || nextChar() != 's'.code || nextChar() != 'e'.code ) {
					pos = save;
					invalidChar();
				}
				return false;
			case 'n'.code:
				var save = pos;
				if( nextChar() != 'u'.code || nextChar() != 'l'.code || nextChar() != 'l'.code ) {
					pos = save;
					invalidChar();
				}
				return null;
			case '"'.code:
				return parseString();
			case '0'.code, '1'.code,'2'.code,'3'.code,'4'.code,'5'.code,'6'.code,'7'.code,'8'.code,'9'.code,'-'.code:
				pos--;
				if( !reg_float.match(str.substr(pos)) )
					throw "Invalid float at position "+pos;
				var v = reg_float.matched(0);
				pos += v.length;
				return Std.parseFloat(v);
			default:
				invalidChar();
			}
		}
		return null;
	}

	function parseString() {
		var start = pos;
		var buf = new StringBuf();
		while( true ) {
			var c = nextChar();
			if( c == '"'.code )
				break;
			if( c == '\\'.code ) {
				buf.addSub(str,start, pos - start - 1);
				c = nextChar();
				switch( c ) {
				case "r".code: buf.addChar("\r".code);
				case "n".code: buf.addChar("\n".code);
				case "t".code: buf.addChar("\t".code);
				case "b".code: buf.addChar(8);
				case "f".code: buf.addChar(12);
				case "/".code, '\\'.code, '"'.code: buf.addChar(c);
				case 'u'.code:
					var uc = Std.parseInt("0x" + str.substr(pos, 4));
					pos += 4;
					#if (neko || php || cpp)
					if( uc <= 0x7F )
						buf.addChar(uc);
					else if( uc <= 0x7FF ) {
						buf.addChar(0xC0 | (uc >> 6));
						buf.addChar(0x80 | (uc & 63));
					} else if( uc <= 0xFFFF ) {
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
					buf.addChar(uc);
					#end
				default:
					throw "Invalid escape sequence \\" + String.fromCharCode(c) + " at position " + (pos - 1);
				}
				start = pos;
			}
			#if (neko || php || cpp)
			// ensure utf8 chars are not cut
			else if( c >= 0x80 ) {
				pos++;
				if( c >= 0xE0 ) pos += 1 + (c & 32);
			}
			#end
			else if( StringTools.isEOF(c) )
				throw "Unclosed string";
		}
		buf.addSub(str,start, pos - start - 1);
		return buf.toString();
	}

#end

	public static function parse( text : String ) : Dynamic {
		#if (__php && !haxeJSON)
		// don't use because of arrays wrappers
		return untyped __call__("json_encode", value);
		#elseif (flash11 && !haxeJSON)
		return null;
		#else
		return new Json().doParse(text);
		#end
	}

	public static function stringify( value : Dynamic ) : String {
		#if (__php && !haxeJSON)
		// don't use because of arrays wrappers
		return untyped __call__("json_encode", value);
		#elseif (flash11 && !haxeJSON)
		return null;
		#else
		return new Json().toString(value);
		#end
	}

	#if !haxeJSON
		#if js
		static function __init__() untyped {
			if( __js__('typeof(JSON)') != 'undefined' )
				Json = __js__('JSON');
		}
		#end
	#end

}
