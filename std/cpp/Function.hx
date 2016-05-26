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
 package cpp;

@:coreType @:structAccess @:include("cpp/Pointer.h")
extern class Function<T,ABI:cpp.abi.Abi>
{
   public function new(d:Dynamic);

   // Actually a function pointer, but can be called using haxe notation
	public var call(default,null):T;

   @:native("::cpp::Function_obj::getProcAddress")
   static function nativeGetProcAddress<T,ABI:cpp.abi.Abi>(inModule:String, inFunction:String) : Function<T,ABI>;
   public static function getProcAddress<T,ABI:cpp.abi.Abi>(inModule:String, inFunction:String) : Function<T,ABI>
   {
      var autoCast = nativeGetProcAddress(inModule, inFunction);
      return autoCast;
   }

   @:native("::cpp::Function_obj::fromStaticFunction")
   static function nativeFromStaticFunction<T>(inStaticFunction:T) : Callable<T>;
   inline public static function fromStaticFunction<T>(inStaticFunction:T) : Callable<T>
   {
      var autoCast = nativeFromStaticFunction(inStaticFunction);
      return autoCast;
   }

	public function lt(inOther:Function<T,ABI>):Bool;
	public function leq(inOther:Function<T,ABI>):Bool;
	public function gt(inOther:Function<T,ABI>):Bool;
	public function geq(inOther:Function<T,ABI>):Bool;
}



