/*
Copyright (c) 2017-2018 Guillaume Desquesnes, Valentin Lemière

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
package json2object.writer;

import StringTools;

/**
 *  Taken from https://github.com/HaxeFoundation/haxe/blob/875ad19432abc2cec6b345cc49a880f5c7f3c98a/std/haxe/format/JsonPrinter.hx
 */
class StringUtils {
	#if (haxe_ver >= 4)
		public static function quote(s:String) : String {
			#if neko
			if( s.length != neko.Utf8.length(s) ) {
				return quoteUtf8(s);
			}
			#end

			var buf : #if flash flash.utils.ByteArray #else StringBuf #end;
			#if flash
				buf = new flash.utils.ByteArray();
				buf.endian = flash.utils.Endian.BIG_ENDIAN;
				buf.position = 0;
			#else
				buf = new StringBuf();
			#end
			inline function addChar (c:Int) {
				#if flash
				buf.writeByte(c);
				#else
				buf.addChar(c);
				#end
			}
			inline function add (v:String) {
				#if flash
				// argument is not always a string but will be automatically casted
				buf.writeUTFBytes(v);
				#else
				buf.add(v);
				#end
			}

			addChar('"'.code);
			var i = 0;
			#if hl
			var prev = -1;
			#end
			while( true ) {
				var c = StringTools.fastCodeAt(s, i++);
				if( StringTools.isEof(c) ) break;
				switch( c ) {
				case '"'.code: add('\\"');
				case '\\'.code: add('\\\\');
				case '\n'.code: add('\\n');
				case '\r'.code: add('\\r');
				case '\t'.code: add('\\t');
				case 8: add('\\b');
				case 12: add('\\f');
				default:
					#if flash
					if( c >= 128 ) add(String.fromCharCode(c)) else addChar(c);
					#elseif hl
					if( prev >= 0 ) {
						if( c >= 0xD800 && c <= 0xDFFF ) {
							addChar( (((prev - 0xD800) << 10) | (c - 0xDC00)) + 0x10000 );
							prev = -1;
						} else {
							addChar("□".code);
							prev = c;
						}
					} else {
						if( c >= 0xD800 && c <= 0xDFFF )
							prev = c;
						else
							addChar(c);
					}
					#else
					addChar(c);
					#end
				}
			}
			#if hl
			if( prev >= 0 ) addChar("□".code);
			#end
			addChar('"'.code);
			return buf.toString();
		}

		#if neko
		static function quoteUtf8( s : String ) {
			var u = new neko.Utf8();
			neko.Utf8.iter(s,function(c) {
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
			var buf = new StringBuf();
			buf.add('"');
			buf.add(u.toString());
			buf.add('"');
			return buf.toString();
		}
		#end
	#else
		public static function quote(s:String) : String {
			#if (neko || php || cpp)
			if( s.length != haxe.Utf8.length(s) ) {
				return quoteUtf8(s);
			}
			#end
			var buffer = new StringBuf();
			buffer.add('"');
			var i = 0;
			while( true ) {
				var c = StringTools.fastCodeAt(s, i++);
				if( StringTools.isEof(c) ) break;
				switch( c ) {
				case '"'.code: buffer.add('\\"');
				case '\\'.code: buffer.add('\\\\');
				case '\n'.code: buffer.add('\\n');
				case '\r'.code: buffer.add('\\r');
				case '\t'.code: buffer.add('\\t');
				case 8: buffer.add('\\b');
				case 12: buffer.add('\\f');
				default:
					#if flash
					if( c >= 128 ) buffer.add(String.fromCharCode(c)) else buffer.addChar(c);
					#else
					buffer.addChar(c);
					#end
				}
			}
			buffer.add('"');
			return buffer.toString();
		}

		#if (neko || php || cpp)
		static function quoteUtf8( s : String ) {
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
			return '"' + u.toString() + '"';
		}
		#end
	#end
}