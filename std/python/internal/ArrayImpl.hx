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

package python.internal;

import python.lib.Functools;

@:allow(Array)
class ArrayImpl {

	@:ifFeature("dynamic_read.length", "anon_optional_read.length", "python.internal.ArrayImpl.length")
	public static inline function get_length <T>(x:Array<T>):Int return UBuiltins.len(x);

	@:ifFeature("dynamic_read.concat", "anon_optional_read.concat", "python.internal.ArrayImpl.concat")
	public static inline function concat<T>(a1:Array<T>, a2 : Array<T>) : Array<T> {
		return Syntax.binop(a1, "+", a2);
	}

	@:ifFeature("dynamic_read.copy", "anon_optional_read.copy", "python.internal.ArrayImpl.copy")
	public static inline function copy<T>(x:Array<T>) : Array<T> {
		return UBuiltins.list(x);
	}

	@:ifFeature("dynamic_read.iterator", "anon_optional_read.iterator", "python.internal.ArrayImpl.iterator")
	public static inline function iterator<T>(x:Array<T>) : Iterator<T> {
		return new HaxeIterator(Syntax.callField(x, "__iter__"));
	}

	@:ifFeature("dynamic_read.indexOf", "anon_optional_read.indexOf", "python.internal.ArrayImpl.indexOf")
	public static function indexOf<T>(a:Array<T>, x : T, ?fromIndex:Int) : Int {
		var len = a.length;
		var l =
			if (fromIndex == null) 0
			else if (fromIndex < 0) len + fromIndex
			else fromIndex;
		if (l < 0) l = 0;
		for (i in l...len) {
			if (unsafeGet(a,i) == x) return i;
		}
		return -1;
	}

	@:ifFeature("dynamic_read.lastIndexOf", "anon_optional_read.lastIndexOf", "python.internal.ArrayImpl.lastIndexOf")
	public static function lastIndexOf<T>(a:Array<T>, x : T, ?fromIndex:Int) : Int {
		var len = a.length;
		var l =
			if (fromIndex == null) len
			else if (fromIndex < 0) len + fromIndex + 1
			else fromIndex+1;
		if (l > len) l = len;
		while (--l > -1) {
			if (unsafeGet(a,l) == x) return l;
		}
		return -1;
	}

	@:access(python.Boot)
	@:ifFeature("dynamic_read.join", "anon_optional_read.join", "python.internal.ArrayImpl.join")
	public static inline function join<T>(x:Array<T>, sep : String ) : String {
		return Boot.arrayJoin(x, sep);
	}

	@:ifFeature("dynamic_read.toString", "anon_optional_read.toString", "python.internal.ArrayImpl.toString")
	public static inline function toString<T>(x:Array<T>) : String {
		return "[" + join(x, ",") + "]";
	}

	@:ifFeature("dynamic_read.pop", "anon_optional_read.pop", "python.internal.ArrayImpl.pop")
	public static inline function pop<T>(x:Array<T>) : Null<T> {
		return if (x.length == 0) null else Syntax.callField(x, "pop");
	}

	@:ifFeature("dynamic_read.push", "anon_optional_read.push", "python.internal.ArrayImpl.push")
	public static inline function push<T>(x:Array<T>, e:T) : Int {
		Syntax.callField(x, "append", e);
		return x.length;
	}

	@:ifFeature("dynamic_read.unshift", "anon_optional_read.unshift", "python.internal.ArrayImpl.unshift")
	public static inline function unshift<T>(x:Array<T>,e : T) : Void {
		x.insert(0,e);
	}

	@:ifFeature("dynamic_read.remove", "anon_optional_read.remove", "python.internal.ArrayImpl.remove")
	public static function remove<T>(x:Array<T>,e : T) : Bool {
		try {
			Syntax.callField(x, "remove", e);
			return true;
		} catch (e:Dynamic) {
			return false;
		}
	}

	@:ifFeature("dynamic_read.shift", "anon_optional_read.shift", "python.internal.ArrayImpl.shift")
	public static inline function shift<T>(x:Array<T>) : Null<T> {
		if (x.length == 0) return null;
		return Syntax.callField(x, "pop", 0);
	}

	@:ifFeature("dynamic_read.slice", "anon_optional_read.slice", "python.internal.ArrayImpl.slice")
	public static inline function slice<T>(x:Array<T>, pos : Int, ?end : Int ) : Array<T> {
		return Syntax.arrayAccess(x, pos, end);
	}
	@:ifFeature("dynamic_read.sort", "anon_optional_read.sort", "python.internal.ArrayImpl.sort")
	public static inline function sort<T>(x:Array<T>, f:T->T->Int) : Void {
		Syntax.callNamedUntyped(Syntax.field(x, "sort"), { key : Functools.cmp_to_key(f) });
	}

	@:ifFeature("dynamic_read.splice", "anon_optional_read.splice", "python.internal.ArrayImpl.splice")
	public static inline function splice<T>(x:Array<T>, pos : Int, len : Int ) : Array<T> {
		if (pos < 0) pos = x.length+pos;
		if (pos < 0) pos = 0;
		var res = Syntax.arrayAccess(x, pos, pos+len);
		Syntax.delete((Syntax.arrayAccess(x, pos, pos+len)));
		return res;
	}

	@:ifFeature("dynamic_read.map", "anon_optional_read.map", "python.internal.ArrayImpl.map")
	public static inline function map<S,T>(x:Array<T>, f : T -> S) : Array<S> {
		return UBuiltins.list(UBuiltins.map(f, cast x));
	}

	@:ifFeature("dynamic_read.filter", "anon_optional_read.filter", "python.internal.ArrayImpl.filter")
	public static inline function filter<T>(x:Array<T>, f : T -> Bool) : Array<T> {
		return UBuiltins.list(UBuiltins.filter(f, cast x));
	}

	@:ifFeature("dynamic_read.insert", "anon_optional_read.insert", "python.internal.ArrayImpl.insert")
	public static inline function insert<T>(a:Array<T>, pos : Int, x : T ) : Void {
		Syntax.callField(a, "insert", pos, x);
	}
	@:ifFeature("dynamic_read.reverse", "anon_optional_read.reverse", "python.internal.ArrayImpl.reverse")
	public static inline function reverse<T>(a:Array<T>) : Void {
		Syntax.callField(a, "reverse");
	}

	@:ifFeature("array_read")
	private static inline function _get<T>(x:Array<T>, idx:Int):T {
		return if (idx > -1 && idx < x.length) unsafeGet(x, idx) else null;
	}

	@:ifFeature("array_write")
	private static inline function _set<T>(x:Array<T>, idx:Int, v:T):T {
		var l = x.length;
		while (l < idx) {
			x.push(null);
			l+=1;
		}
		if (l == idx) {
			x.push(v);
		} else {
			Syntax.assign(Syntax.arrayAccess(x, idx), v);
		}
		return v;
	}

	public static inline function unsafeGet<T>(x:Array<T>,idx:Int):T {
		return Syntax.arrayAccess(x, idx);
	}

	public static inline function unsafeSet<T>(x:Array<T>,idx:Int, val:T):T {
		Syntax.assign(Syntax.arrayAccess(x, idx), val);
		return val;
	}
}