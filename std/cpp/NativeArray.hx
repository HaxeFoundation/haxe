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

extern class NativeArray {

	public static inline function blit<T>( ioDestArray:Array<T>,
		inDestElement:Int, inSourceArray:Array<T>,
		inSourceElement:Int, inElementCount:Int ): Void  {
	untyped ioDestArray.blit(inDestElement, inSourceArray, inSourceElement, inElementCount);
	};

	public static inline function getBase( inArray:Array<Dynamic> ) : ArrayBase {
      return untyped inArray;
   }

	public static inline function address<T>( inArray:Array<T>,inIndex:Int ) : Pointer<T> {
      return Pointer.arrayElem(inArray,inIndex);
   }

	public static inline function setData<T>( inArray:Array<T>,inData:Pointer<T>,inElementCount:Int ) : Void {
      untyped inArray.setData(inData.raw,inElementCount);
   }
	public static inline function setUnmanagedData<T>( inArray:Array<T>,inData:Pointer<T>,inElementCount:Int ) : Void {
      untyped inArray.setUnmanagedData(inData.get_raw(),inElementCount);
   }


	public static inline function zero<T>( ioDestArray:Array<T>, ?inFirst:Int, ?inElements:Int ) : Void {
		untyped ioDestArray.zero(inFirst, inElements);
	};

	public static inline function unsafeGet<T>( inDestArray:Array<T>, inIndex:Int) : T {
		return untyped inDestArray.__unsafe_get(inIndex);
	}

	public static inline function unsafeSet<T>( ioDestArray:Array<T>, inIndex:Int, inValue:T) : T {
		return untyped ioDestArray.__unsafe_set(inIndex,inValue);
	}

	public static inline function memcmp<T>( inArrayA:Array<T>, inArrayB:Array<T>) : Int {
		return untyped inArrayA.memcmp(inArrayB);
	}

	public static inline function setSize<T>( ioArray:Array<T>, inSize:Int) : Array<T> {
		return untyped ioArray.__SetSizeExact(inSize);
   }
}
