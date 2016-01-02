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

@:coreType
@:analyzer(no_simplification)
extern class Pointer<T> extends ConstPointer<T> implements ArrayAccess<T>
{
   @:analyzer(no_simplification)
	public var ref(get,set):T;

   @:analyzer(no_simplification)
   public function get_ref() : T;
   @:analyzer(no_simplification)
   public function set_ref(t:T) : T;


   public static function fromRaw<T>(ptr:RawPointer<T>) : Pointer<T>;

   public static function fromHandle<T>(inHandle:Dynamic,?inKind:String) : Pointer<T>;

   public static function fromPointer<T>(inNativePointer:Dynamic) : Pointer<T>;

   public static function addressOf<T>(inVariable:T) : Pointer<T>;

	public static function arrayElem<T>(array:Array<T>, inElem:Int):Pointer<T>;

   public function get_raw() : RawPointer<T>;

	override public function inc():Pointer<T>;
	override public function dec():Pointer<T>;
	override public function incBy(inT:Int):Pointer<T>;
	override public function add(inT:Int):Pointer<T>;

   @:analyzer(no_simplification)
	public function postIncRef():T;

	public function destroy():Void;
	public function destroyArray():Void;
}

