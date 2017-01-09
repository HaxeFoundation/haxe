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

import python.internal.UBuiltins;
import python.Syntax;

@:native("tuple")
extern class Tuple<X> implements ArrayAccess<X> {
	@:overload(function():Void {})
	function new(a:Array<X>):Void;

	var length(get,never):Int;

	inline function get_length():Int {
		return UBuiltins.len(this);
	}

	inline function toArray():Array<X> {
		return UBuiltins.list(this);
	}
}

@:native("tuple")
extern class Tuple1<A> extends Tuple<Dynamic> {
	static inline function make<A>(a:A):Tuple1<A> return Syntax.tuple(a);
	var _1(get, null):A;
	inline function get__1():A return this[0];
}

@:native("tuple")
extern class Tuple2<A,B> extends Tuple<Dynamic> {
	static inline function make <A,B>(a:A, b:B):Tuple2<A,B> return Syntax.tuple(a,b);
	var _1(get, null):A;
	inline function get__1():A return this[0];
	var _2(get, null):B;
	inline function get__2():B return this[1];
}

@:native("tuple")
extern class Tuple3<A,B,C> extends Tuple<Dynamic> {
	static inline function make <A,B,C>(a:A, b:B,c:C):Tuple3<A,B,C> return Syntax.tuple(a,b,c);
	var _1(get, null):A;
	inline function get__1():A return this[0];
	var _2(get, null):B;
	inline function get__2():B return this[1];
	var _3(get, null):C;
	inline function get__3():C return this[2];
}

@:native("tuple")
extern class Tuple4<A,B,C,D> extends Tuple<Dynamic> {
	static inline function make <A,B,C,D>(a:A, b:B,c:C,d:D):Tuple4<A,B,C,D> return Syntax.tuple(a,b,c,d);
	var _1(get, null):A;
	inline function get__1():A return this[0];
	var _2(get, null):B;
	inline function get__2():B return this[1];
	var _3(get, null):C;
	inline function get__3():C return this[2];
	var _4(get, null):D;
	inline function get__4():D return this[3];
}

@:native("tuple")
extern class Tuple5<A,B,C,D,E> extends Tuple<Dynamic> {
	static inline function make <A,B,C,D,E>(a:A, b:B,c:C,d:D,e:E):Tuple5<A,B,C,D,E> return Syntax.tuple(a,b,c,d,e);
	var _1(get, null):A;
	inline function get__1():A return this[0];
	var _2(get, null):B;
	inline function get__2():B return this[1];
	var _3(get, null):C;
	inline function get__3():C return this[2];
	var _4(get, null):D;
	inline function get__4():D return this[3];
	var _5(get, null):E;
	inline function get__5():E return this[4];
}
