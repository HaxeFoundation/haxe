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

import php.Syntax;
import php.Global;
import php.NativeArray;
import php.NativeIndexedArray;

@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int,T> {

	var data:NativeIndexedArray<T>;

	/**
		Creates a new IntMap.
	**/
	public function new() : Void {
		data = new NativeIndexedArray();
	}

	/**
		See `Map.set`
	**/
	public inline function set( key : Int, value : T ) : Void {
		data[key] = value;
	}

	/**
		See `Map.get`
	**/
	public inline function get( key : Int ) : Null<T> {
		return Syntax.binop(data[key], '??', null);
	}

	/**
		See `Map.exists`
	**/
	public inline function exists( key : Int ) : Bool {
		return Global.array_key_exists(key, data);
	}

	/**
		See `Map.remove`
	**/
	public function remove( key : Int ) : Bool {
		if (Global.array_key_exists(key, data)) {
			Global.unset(data[key]);
			return true;
		}

		return false;
	}

	/**
		See `Map.keys`
	**/
	public inline function keys() : Iterator<Int> {
		return Global.array_keys(data).iterator();
	}

	/**
		See `Map.iterator`
	**/
	public inline function iterator() : Iterator<T> {
		return Global.array_values(data).iterator();
	}

	/**
		See `Map.toString`
	**/
	public function toString() : String {
		var parts = new NativeArray();
		Syntax.foreach(data, function(key:Int, value:T) {
			Global.array_push(parts, '$key => ' + Std.string(value));
		});

		return '{' + Global.implode(', ', parts) + '}';
	}
}
