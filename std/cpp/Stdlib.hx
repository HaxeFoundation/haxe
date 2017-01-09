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

@:include("stdlib.h")
extern class Stdlib
{
   @:native("malloc")
   public static function nativeMalloc(bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("calloc")
   public static function nativeCalloc(bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("realloc")
   public static function nativeRealloc(inPtr:cpp.RawPointer<cpp.Void>,bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("free")
   public static function nativeFree(ptr:cpp.RawPointer<cpp.Void>) : Void { }
   @:native("memcpy")
   public static function nativeMemcpy(dest:cpp.RawPointer<cpp.Void>, src:cpp.RawConstPointer<cpp.Void>, bytes:Int) : Void { }

   @:native("hx::ClassSizeOf") @:templatedCall
   public static function sizeof<T>(t:T) : Int { }

   inline public static function memcpy<DEST,SRC>(dest:cpp.Pointer<DEST>, src:cpp.ConstPointer<SRC>, bytes:Int) : Void
      nativeMemcpy(cast dest.ptr, cast src.ptr, bytes);

   inline public static function malloc<T>(bytes:Int) : cpp.Pointer<T>
      return cast nativeMalloc(bytes);

   inline public static function calloc<T>(bytes:Int) : cpp.Pointer<T>
      return cast nativeCalloc(bytes);

   inline public static function realloc<T>(ioPtr:cpp.Pointer<T>, bytes:Int) : Void
      ioPtr.setRaw( nativeRealloc(cast ioPtr.ptr, bytes) );

   inline public static function free<T>(ptr:cpp.Pointer<T>) : Void
   {
      if (ptr!=null)
      {
         nativeFree(cast ptr.ptr);
         ptr.ptr = null;
      }
   }
}
