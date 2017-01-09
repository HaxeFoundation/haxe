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

import haxe.extern.AsVar;

@:coreType
@:analyzer(as_var)
extern class Pointer<T> extends ConstPointer<T> implements ArrayAccess<T>
{
   public var ref(get,set):Reference<T>;

   public function get_ref() : Reference<T>;
   public function set_ref(t:T) : Reference<T>;

   public function setAt(inIndex:Int, value:T):Void;

   public static function fromRaw<T>(ptr:RawPointer<T>) : Pointer<T>;

   @:native("::cpp::Pointer_obj::fromHandle")
   static function nativeFromHandle<T>(inHandle:Dynamic,?inKind:String):AutoCast;
   inline public static function fromHandle<T>(inHandle:Dynamic,?inKind:String) : Pointer<T>
   {
     return cast nativeFromHandle(inHandle,inKind);
   }

   public static function fromPointer<T>(inNativePointer:Dynamic) : Pointer<T>;

   public static function addressOf<T>(inVariable:cpp.Reference<T>) : Pointer<T>;

   public static function endOf<T:{}>(inVariable:T) : Pointer<cpp.Void>;

   @:native("::cpp::Pointer_obj::arrayElem")
   static function nativeArrayElem<T>(array:Array<T>, inElem:Int):AutoCast;
   inline static function arrayElem<T>(array:Array<T>, inElem:Int):Pointer<T>
   {
      return cast nativeArrayElem(array,inElem);
   }

   @:native("::cpp::Pointer_obj::ofArray")
   static function nativeOfArray<T>(array:Array<T>):AutoCast;
   inline public static function ofArray<T>(array:Array<T>):Pointer<T>
   {
     return cast nativeOfArray(array);
   }

   inline public function toUnmanagedArray(elementCount:Int) : Array<T>
   {
      var result = new Array<T>();
      NativeArray.setUnmanagedData(result,this,elementCount);
      return result;
   }

   inline public function toUnmanagedVector(elementCount:Int) : haxe.ds.Vector<T>
      return cast toUnmanagedArray(elementCount);


   override public function inc():Pointer<T>;
   override public function dec():Pointer<T>;
   override public function incBy(inT:Int):Pointer<T>;
   override public function add(inT:Int):Pointer<T>;

   public function postIncRef():Reference<T>;

   public function destroy():Void;
   public function destroyArray():Void;
}

