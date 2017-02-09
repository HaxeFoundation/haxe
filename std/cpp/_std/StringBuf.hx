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
import cpp.NativeString;
using cpp.NativeArray;

@:coreApi
class StringBuf {
   private var b : Array<String>;
   public var length(get,never) : Int;
   var charBuf:Array<cpp.Char>;

   public function new() : Void {
   }

   private function charBufAsString() : String
   {
      var len = charBuf.length;
      charBuf.push(0);
      return NativeString.fromGcPointer( charBuf.address(0), len );
   }

   private function flush() : Void{
      if (b==null)
         b = [charBufAsString()];
      else
         b.push( charBufAsString() );
      charBuf = null;
   }
   function get_length() : Int {
      var len = 0;
      if (charBuf!=null)
         len = charBuf.length;
      if (b!=null)
         for(s in b)
            len += s==null ? 4 : s.length;
      return len;
   }

   public inline function add<T>( x : T ) : Void {
      if (charBuf!=null) flush();
      if (b==null)
         b = [Std.string(x)];
      else
         b.push(Std.string(x));
   }

   public #if !cppia inline #end function addSub( s : String, pos : Int, ?len : Int ) : Void {
      if (charBuf!=null) flush();
      if (b==null)
         b = [s.substr(pos,len)];
      else
         b.push(s.substr(pos,len));
   }

   public #if !cppia inline #end function addChar( c : Int ) : Void {
      if (charBuf==null) charBuf = new Array<cpp.Char>();
      charBuf.push(c);
   }

   public function toString() : String {
      if (charBuf!=null)
         flush();
      if (b==null || b.length==0)
         return "";
      if (b.length==1)
         return b[0];
      return b.join("");
   }

}
