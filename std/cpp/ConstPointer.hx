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

@:coreType @:include("cpp/Pointer.h") @:native("cpp.Pointer") @:analyzer(as_var)
extern class ConstPointer<T>
{
   // ptr actually returns the pointer - not strictly a 'T' - for pointers to smart pointers
   // Use value or ref to get dereferenced value
   public var ptr:Star<T>;

   public var value(get,never):T;

   // Typecast to non-const
   public var raw(get,never):RawPointer<T>;

   // const version
   public var constRaw(get,never):RawConstPointer<T>;

   public function get_value() : Reference<T>;

   public function get_constRaw() : RawConstPointer<T>;
   public function get_raw() : RawPointer<T>;


   public function lt(inOther:ConstPointer<T>):Bool;
   public function leq(inOther:ConstPointer<T>):Bool;
   public function gt(inOther:ConstPointer<T>):Bool;
   public function geq(inOther:ConstPointer<T>):Bool;

   public function setRaw<O>(ptr:RawPointer<O>) : Void;

   public static function fromRaw<T>(ptr:RawConstPointer<T>) : ConstPointer<T>;


   public static function fromPointer<T>(inNativePointer:Dynamic) : ConstPointer<T>;

   public function reinterpret<Other>():Pointer<Other>;

   public function rawCast<Other>():RawPointer<Other>;

   public function at(inIndex:Int):Reference<T>;

   public function inc():ConstPointer<T>;
   public function dec():ConstPointer<T>;
   public function postIncVal():Reference<T>;
   public function incBy(inT:Int):ConstPointer<T>;
   public function add(inT:Int):ConstPointer<T>;

}

