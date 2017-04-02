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
 package haxe.ds;

@:coreApi
class WeakMap<K:{},V> extends flash.utils.Dictionary implements haxe.Constraints.IMap<K,V> {

	public function new() {
		super(true);
	}

	public inline function get( key : K ) : Null<V> {
		return untyped this[key];
	}

	public inline function set( key : K, value : V ):Void {
		untyped this[key] = value;
	}

	public inline function exists( key : K ) : Bool {
		return untyped this[key] != null;
	}

	public function remove( key : K ):Bool {
		var has = exists(key);
		untyped __delete__(this, key);
		return has;
	}

	#if as3

 	public function keys() : Iterator<K> {
		return untyped __keys__(this).iterator();
 	}

 	public function iterator() : Iterator<V> {
		var ret = [];
		for (i in keys())
			ret.push(get(i));
		return ret.iterator();
 	}
	#else

	public function keys() : Iterator<K> {
		return NativePropertyIterator.iterator(this);
	}

	public function iterator() : Iterator<V> {
		return NativeValueIterator.iterator(this);
	}

	#end

	public function toString() : String {
		var s = "";
		var it = keys();
		for( i in it ) {
			s += (s == "" ? "" : ",") + Std.string(i);
			s += " => ";
			s += Std.string(get(i));
		}
		return s + "}";
	}
}

private class NativePropertyIterator {
	var collection:Dynamic;
	var index:Int = 0;

	public static inline function iterator(collection:Dynamic):NativePropertyIterator {
		var result = new NativePropertyIterator();
		result.collection = collection;
		return result;
	}

	function new() {}

	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}

	public inline function next():Dynamic {
		var i = index;
		var result = untyped __forin__(collection, i);
		index = i;
		return result;
	}
}

private class NativeValueIterator {
	var collection:Dynamic;
	var index:Int = 0;

	public static inline function iterator(collection:Dynamic):NativeValueIterator {
		var result = new NativeValueIterator();
		result.collection = collection;
		return result;
	}

	function new() {}

	public inline function hasNext():Bool {
		var c = collection;
		var i = index;
		var result = untyped __has_next__(c, i);
		collection = c;
		index = i;
		return result;
	}

	public inline function next():Dynamic {
		var i = index;
		var result = untyped __foreach__(collection, i);
		index = i;
		return result;
	}
}
