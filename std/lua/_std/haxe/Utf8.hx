/*
 * Copyright (C)2005-2016 Haxe Foundation
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

    public static function iter( s : String, chars : Int -> Void ) {
		var cur = 0;
		while (cur < s.length){
			var code = s.charCodeAt(cur);
			var width = charWidth(code);
			var l = (code << 6)  | s.charCodeAt(cur+1);
			trace(l + " is the value for l");
			switch(width){
			case 1 : chars(code);
			case 2 : chars((code << 6)  | s.charCodeAt(cur+1));
			case 3 : chars((code << 12) | (s.charCodeAt(cur+1) << 6) | s.charCodeAt(cur+2));
			}
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
		if (bytes == 1){
			return code;
		} else if (bytes == 2){
			return ((code & 0x1F) << 6) | (s.charCodeAt(pos+1) & 0x3F);
		} else if (bytes == 3){
			return ((code & 0x0F) << 12) | (((s.charCodeAt(pos+1) & 0x3F) << 6) | (s.charCodeAt(pos+2) & 0x3F));
		} else {
			return null;
		}
    }

    public static function validate( s : String ) : Bool {
		if (s == null) return false;
		var cur = 0;
		while (cur < s.length){
			var code = s.charCodeAt(cur);
			var width = charWidth(code);
			var expectedLen = 0;

			 if ((code & 0x10000000) == 0x00000000) expectedLen = 1;
			else if ((code & 0x11100000) == 0x11000000) expectedLen = 2;
			else if ((code & 0x11110000) == 0x11100000) expectedLen = 3;
			else if ((code & 0x11111000) == 0x11110000) expectedLen = 4;
			else if ((code & 0x11111100) == 0x11111000) expectedLen = 5;
			else if ((code & 0x11111110) == 0x11111100) expectedLen = 6;
			else return false;

			if (cur + expectedLen > s.length) return false;

			for (i in (cur + 1)...expectedLen) {
			if ((s.charCodeAt(i) & 0x11000000) != 0x10000000) {
				return false;
			}
			}

			cur += width;
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

