/*
 * Copyright (C)2005-2012 Haxe Foundation
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
	var buf : #if flash9 flash.utils.ByteArray #else StringBuf #end;
	var str : String;
	var pos : Int;

	function new() {
	}

	@:extern inline function addChar(c:Int) {
		#if flash9
		buf.writeByte(c);
		#else
		buf.addChar(c);
		#end
	}

	@:extern inline function add(v:String) {
		#if flash9
		// argument is not always a string but will be automatically casted
		buf.writeUTFBytes(v);
		#else
		buf.add(v);
		#end
	}

	function toString(v:Dynamic) {
		#if flash9
		buf = new flash.utils.ByteArray();
		buf.endian = flash.utils.Endian.BIG_ENDIAN;
		buf.position = 0;
		#else
		buf = new StringBuf();
		#end
		toStringRec(v);
		return buf.toString();
	}

	function fieldsString( v : Dynamic, fields : Array<String> )
	{
		var first = true;
		addChar('{'.code);
		for( f in fields ) {
			var value = Reflect.field(v,f);
			if( Reflect.isFunction(value) ) continue;
			if( first ) first = false else addChar(','.code);
			quote(f);
			addChar(':'.code);
			toStringRec(value);
		}
		addChar('}'.code);
	}

	#if flash9
	function classString ( v : Dynamic ) {
		fieldsString(v,Type.getInstanceFields(Type.getClass(v)));
	}
	#end

	function objString( v : Dynamic ) {
		fieldsString(v,Reflect.fields(v));
	}

	function toStringRec(v:Dynamic) {
		switch( Type.typeof(v) ) {
		case TUnknown:
			add('"???"');
		case TObject:
			objString(v);
		case TInt:
			add(v);
		case TFloat:
			add(v+1==v ? null : v);
		case TFunction:
			add('"<fun>"');
		case TClass(c):
			if( c == String )
				quote(v);
			else if( c == Array ) {
				var v : Array<Dynamic> = v;
				addChar('['.code);
				var len = v.length;
				if( len > 0 ) {
					toStringRec(v[0]);
					var i = 1;
					while( i < len ) {
						addChar(','.code);
						toStringRec(v[i++]);
					}
				}
				addChar(']'.code);
			} else if( c == Hash ) {
				var v : Hash<Dynamic> = v;
				var o = {};
				for( k in v.keys() )
					Reflect.setField(o,k,v.get(k));
				objString(o);
			} else
				#if flash9
				classString(v);
				#else
				objString(v);
				#end
		case TEnum(_):
			var i : Dynamic = Type.enumIndex(v);
			add(i);
		case TBool:
			add(#if php (v ? 'true' : 'false') #else v #end);
		case TNull:
			add('null');
		}
	}

	function quote( s : String ) {
		#if (neko || php || cpp)
		if( s.length != haxe.Utf8.length(s) ) {
			quoteUtf8(s);
			return;
		}
		#end
		addChar('"'.code);
		var i = 0;
		while( true ) {
			var c = StringTools.fastCodeAt(s,i++);
			if( StringTools.isEof(c) ) break;
			switch( c ) {
			case '"'.code: add('\\"');
			case '\\'.code: add('\\\\');
			case '\n'.code: add('\\n');
			case '\r'.code: add('\\r');
			case '\t'.code: add('\\t');
			case 8: add('\\b');
			case 12: add('\\f');
			default: addChar(c);
			}
		}
		addChar('"'.code);
	}

	#if (neko || php || cpp)
	function quoteUtf8( s : String ) {
		var u = new haxe.Utf8();
		haxe.Utf8.iter(s,function(c) {
			switch( c ) {
			case '\\'.code, '"'.code: u.addChar('\\'.code); u.addChar(c);
			case '\n'.code: u.addChar('\\'.code); u.addChar('n'.code);
			case '\r'.code: u.addChar('\\'.code); u.addChar('r'.code);
			case '\t'.code: u.addChar('\\'.code); u.addChar('t'.code);
			case 8: u.addChar('\\'.code); u.addChar('b'.code);
			case 12: u.addChar('\\'.code); u.addChar('f'.code);
			default: u.addChar(c);
			}
		});
		buf.add('"');
		buf.add(u.toString());
		buf.add('"');
	}
	#end

	function doParse( str : String ) {
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
				return parseNumber(c);
			default:
				invalidChar();
			}
		}
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
				if( c >= 0xFC ) pos += 4;
				else if( c >= 0xF8 ) pos += 3;
				else if( c >= 0xF0 ) pos += 2;
				else if( c >= 0xE0 ) pos++;
			}
			#end
			else if( StringTools.isEof(c) )
				throw "Unclosed string";
		}
		buf.addSub(str,start, pos - start - 1);
		return buf.toString();
	}

	function invalidNumber( start : Int ) {
		throw "Invalid number at position "+start+": " + str.substr(start, pos - start);
	}

	inline function parseNumber( c : Int ) {
		var start = pos - 1;
		var minus = c == '-'.code, digit = !minus, zero = c == '0'.code;
		var point = false, e = false, pm = false, end = false;
		while( true ) {
			c = nextChar();
			switch( c ) {
				case '0'.code :
					if (zero && !point) invalidNumber(start);
					if (minus) {
						minus = false; zero = true;
					}
					digit = true;
				case '1'.code,'2'.code,'3'.code,'4'.code,'5'.code,'6'.code,'7'.code,'8'.code,'9'.code :
					if (zero && !point) invalidNumber(start);
					if (minus) minus = false;
					digit = true; zero = false;
				case '.'.code :
					if (minus || point) invalidNumber(start);
					digit = false; point = true;
				case 'e'.code, 'E'.code :
					if (minus || zero || e) invalidNumber(start);
					digit = false; e = true;
				case '+'.code, '-'.code :
					if (!e || pm) invalidNumber(start);
					digit = false; pm = true;
				default :
					if (!digit) invalidNumber(start);
					pos--;
					end = true;
			}
			if (end) break;
		}
		var f = Std.parseFloat(str.substr(start, pos - start));
		var i = Std.int(f);
		return if( i == f ) i else f;
	}

#end

	public static function parse( text : String ) : Dynamic {
		#if (php && !haxeJSON)
		return phpJsonDecode(text);
		#elseif (flash11 && !haxeJSON)
		return null;
		#else
		return new Json().doParse(text);
		#end
	}

	public static function stringify( value : Dynamic ) : String {
		#if (php && !haxeJSON)
		return phpJsonEncode(value);
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

	#if php
	public static function phpJsonDecode(json:String):Dynamic {
		var val = untyped __call__("json_decode", json);
		return convertAfterDecode(val);
	}

	static function convertAfterDecode(val:Dynamic):Dynamic {
		var arr:php.NativeArray;
		if (untyped __call__("is_object", val)) {
			arr = phpMapArray(php.Lib.associativeArrayOfObject(val), convertAfterDecode);
			return untyped __call__("_hx_anonymous", arr);
		}
		else if (untyped __call__("is_array", val)) {
			arr = phpMapArray(val, convertAfterDecode);
			return php.Lib.toHaxeArray(arr);
		}
		else
			return val;
	}

	public static function phpJsonEncode(val:Dynamic):String {
		var json = untyped __call__("json_encode", convertBeforeEncode(val));
		if (untyped __physeq__(json, false))
			return throw "invalid json";
		else
			return json;
	}

	static function convertBeforeEncode(val:Dynamic):Dynamic {
		var arr:php.NativeArray;
		if (untyped __call__("is_object", val)) {
			switch (untyped __call__("get_class", val)) {
				case "_hx_anonymous", "stdClass" : arr = php.Lib.associativeArrayOfObject(val);
				case "_hx_array" : arr = php.Lib.toPhpArray(val);
				case "Date" : return Std.string(val); //.split(" ").join("T"); //better with "T"?
				case "HList" : arr = php.Lib.toPhpArray(Lambda.array(val)); //convert List to array?
				case "_hx_enum" : return Type.enumIndex(val);
				case "Hash", "IntHash" : arr = php.Lib.associativeArrayOfHash(val);
				default : arr = php.Lib.associativeArrayOfObject(val);
			}
		}
		else if (untyped __call__("is_array", val)) arr = val;
		else
			return val;
		return phpMapArray(arr, convertBeforeEncode);
	}

	inline static function phpMapArray(arr:php.NativeArray
	, func:Dynamic->Dynamic):php.NativeArray {
		return untyped __call__("array_map", func, arr);
	}

	#end

}
