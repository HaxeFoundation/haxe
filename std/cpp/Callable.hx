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


@:noPackageRestrict @:callable
typedef CallableData<T> = T;

// The generator intercepts this type and converts it to a cpp.Function<T> on cpp
@:noPackageRestrict @:callable
#if cpp extern #end
abstract Callable<T>( CallableData<T> )
{
   inline public function new(inValue:T) this = inValue;
   public var call(get,never):CallableData<T>;
   inline function get_call():CallableData<T> return this;

   #if cpp
   @:from
   inline static public function fromFunction<F>( func:Function<F,cpp.abi.Abi> ) : Callable<F>
       return new Callable<F>(cast func);
   @:to
   inline public function toFunction() : Function<T,cpp.abi.Abi> return cast this;


   inline public static function getProcAddress<T,ABI:cpp.abi.Abi>(inModule:String, inFunction:String) : Function<T,ABI>
      return Function.getProcAddress(inModule, inFunction);

   inline public static function fromStaticFunction<T>(inStaticFunction:T) : Callable<T>
      return Function.fromStaticFunction(inStaticFunction);

   inline public function lt(inOther:Callable<T>):Bool return toFunction().lt(inOther.toFunction());
   inline public function leq(inOther:Callable<T>):Bool return toFunction().leq(inOther.toFunction());
   inline public function gt(inOther:Callable<T>):Bool return toFunction().gt(inOther.toFunction());
   inline public function geq(inOther:Callable<T>):Bool return toFunction().geq(inOther.toFunction());
   #end
}


