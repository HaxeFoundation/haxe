/*
 * Copyright (C)2005-2019 Haxe Foundation
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

package flash;

/**
	The Vector class is very similar to Array but is only supported by the Flash Player 10+
**/
@:require(flash10) extern class Vector<T> implements ArrayAccess<T> {
	var length:Int;
	var fixed:Bool;

	function new(?length:UInt, ?fixed:Bool):Void;
	function concat(?a:Vector<T>):Vector<T>;
	function join(sep:String):String;
	function pop():Null<T>;
	function push(x:T):Int;
	function reverse():Void;
	function shift():Null<T>;
	function unshift(x:T):Void;
	function slice(?pos:Int, ?end:Int):Vector<T>;
	function sort(f:T->T->Int):Void;
	function splice(pos:Int, len:Int):Vector<T>;
	function toString():String;
	function indexOf(x:T, ?from:Int):Int;
	function lastIndexOf(x:T, ?from:Int):Int;

	#if flash19
	function insertAt(index:Int, element:T):Void;
	#else
	inline function insertAt(index:Int, element:T):Void {
		(cast this).splice(index, 0, element);
	}
	#end
	@:require(flash19) function removeAt(index:Int):T;

	inline static function ofArray<T>(v:Array<T>):Vector<T> {
		return untyped __vector__(v);
	}

	inline static function convert<T, U>(v:Vector<T>):Vector<U> {
		return untyped __vector__(v);
	}

	/**
		Get a run-time value referencing the `Vector` class with concrete type parameters.

		Normally in Haxe, for most of the types, type parameters are eliminated at run-time,
		so there is no way to check if a value is of a type with specific type parameters.

		However, on the Flash target, the `flash.Vector<T>` values carry type parameter
		information at run-time all the type-checks (such as `Std.isOfType` and `Std.downcast`) on them
		must be done using a `Class<T>` value that also carries the type parameters. However,
		Haxe syntax does not allow creating such values and this function exists to mitigate
		this limitation.

		It should be used as such:
		```haxe
		var specificVectorType:Class<Vector<Int>> = Vector.typeReference();
		trace(Std.isOfType(vec, specificVectorType));
		```
		or using the type-check syntax:
		```haxe
		trace(Std.isOfType(vec, (Vector.typeReference() : Class<Vector<Int>>)));
		```

		It's also helpful when working with native Flash libraries, that receive Class instances:
		```haxe
		new Signal((Vector.typeReference() : Class<Vector<Int>>));
		```
	**/
	inline static function typeReference<T>():Class<Vector<T>> {
		return untyped __vector__();
	}
}
