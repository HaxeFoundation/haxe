/*
 * Copyright (C)2005-2017 Haxe Foundation
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

import lua.NativeStringTools;

class Utf8 {

    var __b : String;

    public function new( ?size : Int ) {
		__b = "";
    }

    public inline function addChar( c : Int ) : Void {
		__b += char(c);
    }

    public inline function toString() : String {
		return __b;
    }

    static inline function decodeChar( s : String, pos : Int, code : Int, width : Int ) {
        return
            if (width == 1)
                code;
            else if (width == 2)
                ((code & 0x3F) << 6) |
                (s.charCodeAt(pos+1) & 0x7F);
            else if (width == 3)
                ((code & 0x1F) << 12) |
                ((s.charCodeAt(pos+1) & 0x7F) << 6) |
                (s.charCodeAt(pos+2) & 0x7F);
            else
                ((code & 0x0F) << 18) |
                ((s.charCodeAt(pos+1) & 0x7F) << 12) |
                ((s.charCodeAt(pos+2) & 0x7F) << 6) |
                (s.charCodeAt(pos+3) & 0x7F);
    }

    public static function iter( s : String, chars : Int -> Void ) {
		var cur = 0;
		while (cur < s.length){
			var code = s.charCodeAt(cur);
			var width = charWidth(code);
			chars( decodeChar( s, cur, code, width ) );
			cur += width;
		}
    }

    public static function encode( s : String ) : String {
		// ported from : http://phpjs.org/functions/utf8_encode/
		if (s == null ) {
			return '';
		}
		var string = (s + ''); // .replace(/\r\n/g, "\n").replace(/\r/g, "\n");
		var utftext = '';
		var start = 0;
		var end = 0;
		var n = 0;
		while (n < s.length) {
			var c1 = string.charCodeAt(n);
			var enc = null;

			if (c1 < 128) {
			end++;
			} else if (c1 > 127 && c1 < 2048) {
			enc = String.fromCharCode( (c1 >> 6) | 192)
				+ String.fromCharCode( (c1 & 63) | 128);
			} else if ((c1 & 0xF800) != 0xD800) {
			enc = String.fromCharCode( (c1 >> 12) | 224)
				+ String.fromCharCode( ((c1 >> 6) & 63) | 128)
				+ String.fromCharCode( (c1 & 63) | 128);
			} else { // surrogate pairs
			if ((c1 & 0xFC00) != 0xD800) {
				throw 'Unmatched trail surrogate at ' + n;
			}
			var c2 = string.charCodeAt(++n);
			if ((c2 & 0xFC00) != 0xDC00) {
				throw 'Unmatched lead surrogate at ' + (n - 1);
			}
			c1 = ((c1 & 0x3FF) << 10) + (c2 & 0x3FF) + 0x10000;
			enc = String.fromCharCode( (c1 >> 18) | 240)
				+ String.fromCharCode( ((c1 >> 12) & 63) | 128)
				+ String.fromCharCode(((c1 >> 6) & 63) | 128)
				+ String.fromCharCode((c1 & 63) | 128);
			}
			if (enc != null) {
			if (end > start) {
				utftext += string.substring(start, end);
			}
			utftext += enc;
			start = end = n + 1;
			}
			n++;
		}

		if (end > start) {
			utftext += string.substring(start, s.length);
		}

		return utftext;

    }

    public static function decode( s : String ) : String {
		var ret = new StringBuf();
		iter(s, function(c){
			if( c == 8364 ) // euro symbol
			c = 164;
			else if( c > 255 ){
			// throw new RangeError('Utf8 decode invalid character ($c)');
			throw 'Utf8::decode invalid character ($c)';
			}

			if (c != 0xFEFF) // BOM
			ret.add(String.fromCharCode(c));
		});
		return ret.toString();
    }

    public static inline function charCodeAt( s : String, index : Int ) : Int {
		var cur_idx = 0;
		var pos = 0;
		for (i in 0...index){
			pos += charWidth(s.charCodeAt(pos));
		}
		var ret = 0;
		var code = s.charCodeAt(pos);
		var bytes = charWidth(code);
		return decodeChar( s, pos, code, bytes );
    }

    public static function validate( s : String ) : Bool {
		if (s == null) return false;
		var cur = 0;
		while (cur < s.length){
			var c1 = s.charCodeAt(cur++);
			if (c1 < 0x80) continue;
			if (c1 < 0xC0) return false;
			if (s.length <= cur) return false;
			var c2 = s.charCodeAt(cur++);
			if (c1 < 0xE0) {
				if ((c1 & 0x1E != 0) && (c2 & 0xC0 == 0x80)) continue;
				return false;
			}
			if (s.length <= cur) return false;
			var c3 = s.charCodeAt(cur++);
			if (c1 < 0xF0) {
				if (((c1 & 0x0F != 0) || (c2 & 0x20 != 0)) && (c2 & 0xC0 == 0x80) && (c3 & 0xC0 == 0x80)
						&& !(c1 == 0xED && 0xA0 <= c2 && c2 <= 0xBF))
					continue;
				return false;
			}
			if (s.length <= cur) return false;
			var c4 = s.charCodeAt(cur++);
			if (c1 < 0xF8) {
				if (((c1 & 0x07 != 0) || (c2 & 0x30 != 0)) && (c2 & 0xC0 == 0x80) && (c3 & 0xC0 == 0x80) && (c4 & 0xC0 == 0x80)
						&& !((c1 == 0xF4 && c2 > 0x8F) || c1 > 0xF4))
					continue;
				return false;
			}
			return false;
		}
		return true;
    }

    public static inline function length( s : String ) : Int {
		var pos = 0;
		var len = 0;
		while (pos < s.length){
			pos += charWidth(s.charCodeAt(pos));
			len++;
		}
		return len;
    }

    public static function compare( a : String, b : String ) : Int {
		return a > b ? 1 : (a == b ? 0 : -1);
    }

    public static inline function sub( s : String, pos : Int, len : Int ) : String {
		var startpos = 0;
		var ret = new StringBuf();
		for (i in 0...pos){
			startpos += charWidth(s.charCodeAt(startpos));
		}
		var endpos = startpos;
		for (i in 0...len){
			endpos += charWidth(s.charCodeAt(endpos));
		}
		return s.substring(startpos, endpos);
    }

    static function charWidth(c:Int) : Int {
		return   if (c >  0   && c <= 127) 1;
			else if (c >= 194 && c <= 223) 2;
			else if (c >= 224 && c <= 239) 3;
			else if (c >= 240 && c <= 244) 4;
			else null;
    }

    public static function char( unicode : Int ) : String {
		if (unicode <= 0x7F) {
			return String.fromCharCode(unicode);
		} else if (unicode <= 0x7FF) {
			var b0 = 0xC0 + Math.floor(unicode / 0x40);
			var b1 = 0x80 + (unicode % 0x40);
			return NativeStringTools.char(b0, b1);
		} else if (unicode <= 0xFFFF) {
			var b0 = 0xE0 +  Math.floor(unicode / 0x1000);
			var b1 = 0x80 + (Math.floor(unicode / 0x40) % 0x40);
			var b2 = 0x80 + (unicode % 0x40);
			return NativeStringTools.char(b0, b1, b2);
		} else if (unicode <= 0x10FFFF) {
			var code = unicode;
			var b3   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b2   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b1   = 0x80 + (code % 0x40);
			code     = Math.floor(code / 0x40);
			var b0   = 0xF0 + code;

			return NativeStringTools.char(b0, b1, b2, b3);
		} else {
			throw 'Unicode greater than U+10FFFF';
		}
    }
}

