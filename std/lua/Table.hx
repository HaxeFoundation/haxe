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

package lua;

/**
	This library provides generic functions for table manipulation.
**/
// TODO: use an abstract here?

@:native("_G.table")
extern class Table<A, B> implements ArrayAccess<B> implements Dynamic<B> {
	@:pure public static function create<A, B>(?arr:Array<B>, ?hsh:Dynamic):Table<A, B>;

	public inline static function fromArray<T>(arr:Array<T>):Table<Int, T> {
		var ret = Table.create();
		for (idx in 0...arr.length) {
			ret[idx + 1] = arr[idx];
		}
		return ret;
	}

	public inline static function fromMap<A, B>(map:Map<A, B>):Table<A, B> {
		var ret = Table.create();
		for (k in map.keys()) {
			ret[untyped k] = map.get(k);
		}
		return ret;
	}

	public inline static function fromDynamic<A, B>(dyn:Dynamic):Table<A, B> {
		var ret = Table.create();
		for (f in Reflect.fields(dyn)) {
			ret[untyped f] = Reflect.field(dyn, f);
		}
		return ret;
	}

	@:overload(function<A, B>(table:Table<A, B>):Void {})
	public static function concat<A, B>(table:Table<A, B>, ?sep:String, ?i:Int, ?j:Int):String;

	public static function foreach<A, B>(table:Table<A, B>, f:A->B->Void):Void;
	public static function foreachi<A, B>(table:Table<A, B>, f:A->B->Int->Void):Void;

	public static function sort<A, B>(table:Table<A, B>, ?order:A->A->Bool):Void;

	@:overload(function<B>(table:Table<Int, B>, value:B):Void {})
	public static function insert<B>(table:Table<Int, B>, pos:Int, value:B):Void;

	@:overload(function<B>(table:Table<Int, B>):Void {})
	public static function remove<B>(table:Table<Int, B>, ?pos:Int):Void;

	#if (lua_ver >= 5.2)
	public static function pack<T>(args:haxe.extern.Rest<T>):Table<Int, T>;
	public static function unpack<Int, V>(args:lua.Table<Int, V>, ?min:Int, ?max:Int):Dynamic;
	#end
}

typedef AnyTable = Table<Dynamic, Dynamic>;
