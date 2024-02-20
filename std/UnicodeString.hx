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

import haxe.io.Bytes;
import haxe.io.Encoding;
import haxe.iterators.StringIteratorUnicode;
import haxe.iterators.StringKeyValueIteratorUnicode;

/**
	This abstract provides consistent cross-target unicode support for characters of any width.

	Due to differing internal representations of strings across targets, only the basic
	multilingual plane (BMP) is supported consistently by `String` class.

	This abstract provides API to consistently handle all characters even beyond BMP.

	@see https://haxe.org/manual/std-String-unicode.html
**/
@:forward
@:access(StringTools)
abstract UnicodeString(String) from String to String {
	/**
		Tells if `b` is a correctly encoded UTF8 byte sequence.
	**/
	static public function validate(b:Bytes, encoding:Encoding):Bool {
		switch (encoding) {
			case RawNative:
				throw "UnicodeString.validate: RawNative encoding is not supported";
			case UTF8:
				var data = b.getData();
				var pos = 0;
				var max = b.length;
				while (pos < max) {
					var c:Int = Bytes.fastGet(data, pos++);
					if (c < 0x80) {} else if (c < 0xC2) {
						return false;
					} else if (c < 0xE0) {
						if (pos + 1 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c2 < 0x80 || c2 > 0xBF) {
							return false;
						}
					} else if (c < 0xF0) {
						if (pos + 2 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c == 0xE0) {
							if (c2 < 0xA0 || c2 > 0xBF)
								return false;
						} else {
							if (c2 < 0x80 || c2 > 0xBF)
								return false;
						}
						var c3:Int = Bytes.fastGet(data, pos++);
						if (c3 < 0x80 || c3 > 0xBF) {
							return false;
						}
						c = (c << 16) | (c2 << 8) | c3;
						if (0xEDA080 <= c && c <= 0xEDBFBF) { // surrogate pairs
							return false;
						}
					} else if (c > 0xF4) {
						return false;
					} else {
						if (pos + 3 > max) {
							return false;
						}
						var c2:Int = Bytes.fastGet(data, pos++);
						if (c == 0xF0) {
							if (c2 < 0x90 || c2 > 0xBF)
								return false;
						} else if (c == 0xF4) {
							if (c2 < 0x80 || c2 > 0x8F)
								return false;
						} else {
							if (c2 < 0x80 || c2 > 0xBF)
								return false;
						}
						var c3:Int = Bytes.fastGet(data, pos++);
						if (c3 < 0x80 || c3 > 0xBF) {
							return false;
						}
						var c4:Int = Bytes.fastGet(data, pos++);
						if (c4 < 0x80 || c4 > 0xBF) {
							return false;
						}
					}
				}
				return true;
		}
	}

	#if target.unicode
	/**
		Creates an instance of UnicodeString.
	**/
	public inline function new(string:String):Void {
		this = string;
	}

	/**
		Returns an iterator of the unicode code points.
	**/
	public inline function iterator():StringIteratorUnicode {
		return new StringIteratorUnicode(this);
	}

	/**
		Returns an iterator of the code point indices and unicode code points.
	**/
	public inline function keyValueIterator():StringKeyValueIteratorUnicode {
		return new StringKeyValueIteratorUnicode(this);
	}

	#if target.utf16
	/**
		The number of characters in `this` String.
	**/
	public var length(get, never):Int;

	/**
		Returns the character at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, the empty String `""`
		is returned.
	**/
	public function charAt(index:Int):String {
		if (index < 0)
			return '';
		var unicodeOffset = 0;
		var nativeOffset = 0;
		while (nativeOffset < this.length) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset++);
			if (unicodeOffset == index) {
				return String.fromCharCode(c);
			}
			if (c >= StringTools.MIN_SURROGATE_CODE_POINT) {
				nativeOffset++;
			}
			unicodeOffset++;
		}
		return '';
	}

	/**
		Returns the character code at position `index` of `this` String.

		If `index` is negative or exceeds `this.length`, `null` is returned.
	**/
	public function charCodeAt(index:Int):Null<Int> {
		if (index < 0)
			return null;
		var unicodeOffset = 0;
		var nativeOffset = 0;
		while (nativeOffset < this.length) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset++);
			if (unicodeOffset == index) {
				return c;
			}
			if (c >= StringTools.MIN_SURROGATE_CODE_POINT) {
				nativeOffset++;
			}
			unicodeOffset++;
		}
		return null;
	}

	/**
		@see String.indexOf
	**/
	public function indexOf(str:String, ?startIndex:Int):Int {
		var startIndex:Int = if (startIndex == null || startIndex < 0) {
			0;
		} else {
			startIndex;
		}
		if (str.length == 0) {
			if (startIndex > length) {
				return length;
			}
			return startIndex;
		}

		var unicodeOffset = 0;
		var nativeOffset = 0;
		var matchingOffset = 0;
		var result = -1;
		while (nativeOffset <= this.length) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset);

			if (unicodeOffset >= startIndex) {
				var c2 = StringTools.utf16CodePointAt(str, matchingOffset);
				if (c == c2) {
					if (matchingOffset == 0) {
						result = unicodeOffset;
					}
					matchingOffset++;
					if (c2 >= StringTools.MIN_SURROGATE_CODE_POINT) {
						matchingOffset++;
					}
					if (matchingOffset == str.length) {
						return result;
					}
				} else if (matchingOffset != 0) {
					result = -1;
					matchingOffset = 0;
					continue;
				}
			}

			nativeOffset++;
			if (c >= StringTools.MIN_SURROGATE_CODE_POINT) {
				nativeOffset++;
			}
			unicodeOffset++;
		}
		return -1;
	}

	/**
		Returns the position of the rightmost occurrence of `str` within `this`
		String.

		If `startIndex` is given, the search is performed within the substring
		of `this` String from 0 to `startIndex + str.length`. Otherwise the search
		is performed within `this` String. In either case, the returned position
		is relative to the beginning of `this` String.

		If `str` cannot be found, -1 is returned.
	**/
	public function lastIndexOf(str:String, ?startIndex:Int):Int {
		if (startIndex == null) {
			startIndex = this.length;
		} else if (startIndex < 0) {
			startIndex = 0;
		}

		var unicodeOffset = 0;
		var nativeOffset = 0;
		var result = -1;
		var lastIndex = -1;
		var matchingOffset = 0;
		var strUnicodeLength = (str : UnicodeString).length;
		while (nativeOffset < this.length && unicodeOffset < startIndex + strUnicodeLength) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset);

			var c2 = StringTools.utf16CodePointAt(str, matchingOffset);
			if (c == c2) {
				if (matchingOffset == 0) {
					lastIndex = unicodeOffset;
				}
				matchingOffset++;
				if (c2 >= StringTools.MIN_SURROGATE_CODE_POINT) {
					matchingOffset++;
				}
				if (matchingOffset == str.length) {
					result = lastIndex;
					lastIndex = -1;
				}
			} else if (matchingOffset != 0) {
				lastIndex = -1;
				matchingOffset = 0;
				continue;
			}

			nativeOffset++;
			if (c >= StringTools.MIN_SURROGATE_CODE_POINT) {
				nativeOffset++;
			}
			unicodeOffset++;
		}
		return result;
	}

	/**
		Returns `len` characters of `this` String, starting at position `pos`.

		If `len` is omitted, all characters from position `pos` to the end of
		`this` String are included.

		If `pos` is negative, its value is calculated from the end of `this`
		String by `this.length + pos`. If this yields a negative value, 0 is
		used instead.

		If the calculated position + `len` exceeds `this.length`, the characters
		from that position to the end of `this` String are returned.

		If `len` is negative, the result is unspecified.
	**/
	public function substr(pos:Int, ?len:Int):String {
		if (pos < 0) {
			pos = (this : UnicodeString).length + pos;
			if (pos < 0) {
				pos = 0;
			}
		}
		if (len != null) {
			if (len < 0) {
				len = (this : UnicodeString).length + len;
			}
			if (len <= 0) {
				return "";
			}
		}
		var unicodeOffset = 0;
		var nativeOffset = 0;
		var fromOffset = -1;
		var subLength = 0;
		while (nativeOffset < this.length) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset);

			if (unicodeOffset >= pos) {
				if (fromOffset < 0) {
					if (len == null) {
						return this.substr(nativeOffset);
					}
					fromOffset = nativeOffset;
				}
				subLength++;
				if (subLength >= len) {
					var lastOffset = (c < StringTools.MIN_SURROGATE_CODE_POINT ? nativeOffset : nativeOffset + 1);
					return this.substr(fromOffset, lastOffset - fromOffset + 1);
				}
			}

			nativeOffset += (c >= StringTools.MIN_SURROGATE_CODE_POINT ? 2 : 1);
			unicodeOffset++;
		}
		return (fromOffset < 0 ? "" : this.substr(fromOffset));
	}

	/**
		Returns the part of `this` String from `startIndex` to but not including `endIndex`.

		If `startIndex` or `endIndex` are negative, 0 is used instead.

		If `startIndex` exceeds `endIndex`, they are swapped.

		If the (possibly swapped) `endIndex` is omitted or exceeds
		`this.length`, `this.length` is used instead.

		If the (possibly swapped) `startIndex` exceeds `this.length`, the empty
		String `""` is returned.
	**/
	public function substring(startIndex:Int, ?endIndex:Int):String {
		if (startIndex < 0) {
			startIndex = 0;
		}
		if (endIndex != null) {
			if (endIndex < 0) {
				endIndex = 0;
			}
			if (startIndex == endIndex) {
				return "";
			}
			if (startIndex > endIndex) {
				var tmp = startIndex;
				startIndex = endIndex;
				endIndex = tmp;
			}
		}

		var unicodeOffset = 0;
		var nativeOffset = 0;
		var fromOffset = -1;
		var subLength = 0;
		while (nativeOffset < this.length) {
			var c = StringTools.utf16CodePointAt(this, nativeOffset);

			if (startIndex <= unicodeOffset) {
				if (fromOffset < 0) {
					if (endIndex == null) {
						return this.substr(nativeOffset);
					}
					fromOffset = nativeOffset;
				}
				subLength++;
				if (subLength >= endIndex - startIndex) {
					var lastOffset = (c < StringTools.MIN_SURROGATE_CODE_POINT ? nativeOffset : nativeOffset + 1);
					return this.substr(fromOffset, lastOffset - fromOffset + 1);
				}
			}

			nativeOffset += (c >= StringTools.MIN_SURROGATE_CODE_POINT ? 2 : 1);
			unicodeOffset++;
		}
		return (fromOffset < 0 ? "" : this.substr(fromOffset));
	}

	function get_length():Int {
		var l = 0;
		for (c in new StringIteratorUnicode(this)) {
			l++;
		}
		return l;
	}
	#end
	#end
	@:op(A < B) static function lt(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A <= B) static function lte(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A > B) static function gt(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A >= B) static function gte(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A == B) static function eq(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A != B) static function neq(a:UnicodeString, b:UnicodeString):Bool;

	@:op(A + B) static function add(a:UnicodeString, b:UnicodeString):UnicodeString;

	@:op(A += B) static function assignAdd(a:UnicodeString, b:UnicodeString):UnicodeString;

	@:op(A + B) @:commutative static function addString(a:UnicodeString, b:String):UnicodeString;

	@:op(A += B) @:commutative static function assignAddString(a:UnicodeString, b:String):UnicodeString;
}
