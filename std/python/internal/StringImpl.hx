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
package python.internal;

import python.internal.Internal;

@:native("HxString")
class StringImpl {

	@:ifFeature("dynamic_read.split", "anon_optional_read.split", "python.internal.StringImpl.split")
	public static inline function split (s:String, d:String) {
		return if (d == "") UBuiltins.list(s) else Syntax.callField(s, "split", d);
	}

	@:ifFeature("dynamic_read.charCodeAt", "anon_optional_read.charCodeAt", "python.internal.StringImpl.charCodeAt")
	public static function charCodeAt(s:String, index:Int) {
		return
			if (s == null || s.length == 0 || index < 0 || index >= s.length) null
			else UBuiltins.ord(Syntax.arrayAccess(s, index));
	}

	@:ifFeature("dynamic_read.charAt", "anon_optional_read.charAt", "python.internal.StringImpl.charAt")
	public static inline function charAt(s:String, index:Int) {
		return if (index < 0 || index >= s.length) "" else Syntax.arrayAccess(s,index);
	}

	@:ifFeature("dynamic_read.lastIndexOf", "anon_optional_read.lastIndexOf", "python.internal.StringImpl.lastIndexOf")
	public static inline function lastIndexOf(s:String, str:String, ?startIndex:Int):Int {
		if (startIndex == null) {
			return Syntax.callField(s, "rfind", str, 0, s.length);
		} else {

			var i = Syntax.callField(s, "rfind", str, 0, startIndex+1);
			var startLeft = i == -1 ? UBuiltins.max(0, startIndex + 1 - str.length) : i + 1;
			var check = Syntax.callField(s,"find", str, startLeft, s.length);
			if (check > i && check <= startIndex) {
				return check;
			} else {
				return i;
			}
		}
	}

	@:ifFeature("dynamic_read.toUpperCase", "anon_optional_read.toUpperCase", "python.internal.StringImpl.toUpperCase")
	public static inline function toUpperCase (s:String) {
		
		return Syntax.callField(s, "upper");
	}
	@:ifFeature("dynamic_read.toLowerCase", "anon_optional_read.toLowerCase", "python.internal.StringImpl.toLowerCase")
	public static inline function toLowerCase (s:String) {
		return Syntax.callField(s, "lower");
	}
	@:ifFeature("dynamic_read.indexOf", "anon_optional_read.indexOf", "python.internal.StringImpl.indexOf")
	public static inline function indexOf (s:String, str:String, ?startIndex:Int) {
		if (startIndex == null)
			return Syntax.callField(s, "find", str);
		else
			return Syntax.callField(s, "find", str, startIndex);
	}
	@:ifFeature("dynamic_read.toString", "anon_optional_read.toString", "python.internal.StringImpl.toString")
	public static inline function toString (s:String) {
		return s;
	}
	@:ifFeature("dynamic_read.length", "anon_optional_read.length", "python.internal.StringImpl.length")
	public static inline function get_length (s:String) {
		return UBuiltins.len(s);
	}
	@:ifFeature("dynamic_read.fromCharCode", "anon_optional_read.fromCharCode", "python.internal.StringImpl.fromCharCode")
	public static inline function fromCharCode( code : Int ) : String {
		#if doc_gen
		return "";
		#else
		return Syntax.callField('', "join", UBuiltins.map(UBuiltins.chr, cast [code])); // TODO: check cast
		#end
	}
	@:ifFeature("dynamic_read.substring", "anon_optional_read.substring", "python.internal.StringImpl.substring")
	public static function substring( s:String, startIndex : Int, ?endIndex : Int ) : String {
		if (startIndex < 0) startIndex = 0;
		if (endIndex == null) {
			return Syntax.arrayAccessWithTrailingColon(s, startIndex);
		} else {
			if (endIndex < 0) endIndex = 0;
			if (endIndex < startIndex) {

				return Syntax.arrayAccess(s, endIndex, startIndex);
			} else {

				return Syntax.arrayAccess(s, startIndex, endIndex);
			}
		}
	}
	@:ifFeature("dynamic_read.substr", "anon_optional_read.substr", "python.internal.StringImpl.substr")
	public static function substr( s:String, startIndex : Int, ?len : Int ) : String {
		if (len == null) {
			return Syntax.arrayAccessWithTrailingColon(s, startIndex);
		} else {
			if (len == 0) return "";
			return Syntax.arrayAccess(s, startIndex, startIndex+len);
		}

	}
}