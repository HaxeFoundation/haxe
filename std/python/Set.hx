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
package python;

import haxe.extern.Rest;
import python.internal.UBuiltins;
import python.NativeIterator;
import python.NativeIterable;
import python.Syntax;

@:native("set")
extern class Set<T> {
	@:overload(function(?array:Array<T>):Void {})
	function new(?iterable:NativeIterable<T>):Void;

	var length(get,never):Int;
	private inline function get_length():Int {
		return UBuiltins.len(this);
	}

	inline function has(v:T):Bool {
		return Syntax.isIn(v, this);
	}

	function isdisjoint(other:Set<T>):Bool;
	function issubset(other:Set<T>):Bool;
	inline function issubset_proper(other:Set<T>):Bool {
		return Syntax.binop(this, "<", other);
	}
	function issuperset(other:Set<T>):Bool;
	inline function issuperset_proper(other:Set<T>):Bool {
		return Syntax.binop(this, ">", other);
	}
	function union(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function intersection(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function difference(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function symmetric_difference(other:Set<T>):Set<T>;
	function copy():Set<T>;

	function update(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function intersection_update(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function difference_update(other:Set<T>, others:Rest<Set<T>>):Set<T>;
	function symmetric_difference_update(other:Set<T>):Set<T>;

	function add(elem:T):Void;
	function remove(elem:T):Void;
	function discard(elem:T):Void;
	function pop():T;
	function clear():Void;

	inline function iter():NativeIterator<T> {
		return UBuiltins.iter(this);
	}

	inline function iterator():Iterator<T> {
		return iter();
	}

	private function __iter__():NativeIterator<T>;
}
