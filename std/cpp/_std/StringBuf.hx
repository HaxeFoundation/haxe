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
import cpp.NativeString;
using cpp.NativeArray;

@:coreApi
class StringBuf {
	private var b : Array<String>;
	public var length(get,never) : Int;
	var charBuf:Array<cpp.Char>;

	public function new() : Void {
		b = new Array();
	}

   private function flush() : Void{
      b.push( NativeString.fromGcPointer( charBuf.address(0), charBuf.length ) );
      charBuf = null;
   }
	function get_length() : Int {
		if (charBuf!=null) flush();
		var len = 0;
		for(s in b)
			len += s==null ? 4 : s.length;
		return len;
	}

	public inline function add<T>( x : T ) : Void {
		if (charBuf!=null) flush();
		b.push(Std.string(x));
	}

	public #if !cppia inline #end function addSub( s : String, pos : Int, ?len : Int ) : Void {
		if (charBuf!=null) flush();
		b.push(s.substr(pos,len));
	}

	public #if !cppia inline #end function addChar( c : Int ) : Void {
		if (charBuf==null) charBuf = new Array<cpp.Char>();
		charBuf.push(c);
	}

	public #if !cppia inline #end function toString() : String {
		if (charBuf!=null) flush();
		return b.join("");
	}

}
