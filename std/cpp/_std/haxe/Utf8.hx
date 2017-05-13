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

using cpp.NativeString;

@:coreApi
class Utf8
{
   var __s:Array<Int>;

	public function new( ?size : Null<Int> ) : Void {
      __s = new Array<Int>();
      if (size!=null && size>0)
         cpp.NativeArray.reserve(__s,size);
	}

	public function addChar( c : Int ) : Void {
      __s.push(c);
	}

	public function toString() : String {
		return untyped __global__.__hxcpp_char_array_to_utf8_string(__s);
	}

   // Incoming string is array of bytes containing possibly invalid utf8 chars
   // Result is the same string with the bytes expanded into utf8 sequences
	public static function encode( s : String ) : String {
		return untyped __global__.__hxcpp_char_bytes_to_utf8_string(s);
	}

   // Incoming string is array of bytes representing valid utf8 chars
   // Result is a string containing the compressed bytes
	public static function decode( s : String ) : String {
		return untyped __global__.__hxcpp_utf8_string_to_char_bytes(s);
	}

	public #if !cppia inline #end static function iter( s : String, chars : Int -> Void ) : Void {
      var src = s.c_str();
      var end = src.add( s.length );

      while(src.lt(end))
         chars(src.ptr.utf8DecodeAdvance());
	}

	public static function charCodeAt( s : String, index : Int ) : Int {
      return s.utf8CharCodeAt(index);
	}

	public static function validate( s : String ) : Bool {
      return s.utf8IsValid();
	}

	public static function length( s : String ) : Int {
      return s.utf8Length();
	}

	public static function compare( a : String, b : String ) : Int {
      return a.compare(b);
	}

	public static function sub( s : String, pos : Int, len : Int ) : String {
      return s.utf8Sub(pos,len);
	}

}


