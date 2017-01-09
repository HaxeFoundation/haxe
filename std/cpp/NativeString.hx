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
 package cpp;

extern class NativeString {

	public static inline function raw( inString:String ) : RawConstPointer<Char> {
      return untyped inString.__s;
   }
	public static inline function c_str( inString:String ) : ConstPointer<Char> {
		return cpp.ConstPointer.fromPointer(untyped inString.__s);
   }
	public static inline function fromPointer(inPtr:ConstPointer<Char> ) : String {
      return untyped __global__.String(inPtr.ptr);
   }
	public static inline function fromGcPointer(inPtr:ConstPointer<Char>, inLen:Int ) : String {
      return untyped __global__.String(inPtr.ptr,inLen);
   }


   @:native("_hx_string_compare")
   public static function compare(inString0:String, inString1:String) : Int return 0;

   @:native("_hx_utf8_char_code_at")
   public static function utf8CharCodeAt(inString:String, inIndex:Int) : Int return 0;

   @:native("_hx_utf8_length")
   public static function utf8Length(inString:String) : Int return 1;

   @:native("_hx_utf8_is_valid")
   public static function utf8IsValid(inString:String) : Bool return false;

   @:native("_hx_utf8_sub")
   public static function utf8Sub(inString:String,charStart:Int, inLen:Int) : String return null;

   @:native("_hx_string_create")
   public static function fromPointerLen(inPtr:ConstPointer<Char>, len:Int ) : String;

   @:native("_hx_utf8_decode_advance")
   public static function utf8DecodeAdvance(reference:Char) : Int  return 0;

}

