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

package haxe.ds;

import js.lib.Object;
import haxe.Constraints.IMap;
import haxe.DynamicAccess;

#if (js_es >= 6)
@:coreApi class StringMap<T> implements IMap<String, T> {
	private var m:js.lib.Map<String, T>;

	public inline function new():Void {
		m = new js.lib.Map();
	}

	public inline function set(key:String, value:T):Void {
		m.set(key, value);
	}

	public inline function get(key:String):Null<T> {
		return m.get(key);
	}

	public inline function exists(key:String):Bool {
		return m.has(key);
	}

	public inline function remove(key:String):Bool {
		return m.delete(key);
	}

	public inline function keys():Iterator<String> {
		return new js.lib.HaxeIterator(m.keys());
	}

	public inline function iterator():Iterator<T> {
		return m.iterator();
	}

	public inline function keyValueIterator():KeyValueIterator<String, T> {
		return m.keyValueIterator();
	}

	public inline function copy():StringMap<T> {
		var copied = new StringMap();
		copied.m = new js.lib.Map(m);
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keyValueIterator();
		for (i in it) {
			s.add(i.key);
			s.add(" => ");
			s.add(Std.string(i.value));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public inline function clear():Void {
		m.clear();
	}

	public inline function size():Int {
		return m.size;
	}
}
#else
@:coreApi class StringMap<T> implements IMap<String, T> {
	var h:Dynamic;

	public inline function new() {
		h = Object.create(null);
	}

	public inline function exists(key:String):Bool {
		return Object.prototype.hasOwnProperty.call(h, key);
	}

	public inline function get(key:String):Null<T> {
		return h[cast key];
	}

	public inline function set(key:String, value:T):Void {
		h[cast key] = value;
	}

	public inline function remove(key:String):Bool {
		return if (exists(key)) {
			js.Syntax.delete(h, key); true;
		} else {
			false;
		}
	}

	public inline function keys():Iterator<String> {
		return new haxe.iterators.ArrayIterator(Object.keys(h));
	}

	public inline function iterator():Iterator<T> {
		return new StringMapValueIterator(h);
	}

	public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new StringMapKeyValueIterator(h);
	}

	public inline function copy():StringMap<T> {
		return createCopy(h);
	}

	public inline function clear():Void {
		h = Object.create(null);
	}

	public inline function toString():String {
		return stringify(h);
	}

	public inline function size():Int {
		var s:Any = 0;
		js.Syntax.code("for( var key in {0} ) {1}++", h, s);
		return s;
	}

	// impl
	static function createCopy<T>(h:Dynamic):StringMap<T> {
		var copy = new StringMap();
		js.Syntax.code("for (var key in {0}) {1}[key] = {0}[key]", h, copy.h);
		return copy;
	}

	@:analyzer(no_optimize)
	static function stringify(h:Dynamic):String {
		var s = "[", first = true;
		js.Syntax.code("for (var key in {0}) {", h);
		js.Syntax.code("\tif ({0}) {0} = false; else {1} += ',';", first, s);
		js.Syntax.code("\t{0} += key + ' => ' + {1}({2}[key]);", s, Std.string, h);
		js.Syntax.code("}");
		return s + "]";
	}
}

private class StringMapValueIterator<T> {
	final h:Dynamic;
	final keys:Array<String>;
	final length:Int;
	var current:Int;

	public inline function new(h:Dynamic) {
		this.h = h;
		keys = Object.keys(h);
		length = keys.length;
		current = 0;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():T {
		return h[cast keys[current++]];
	}
}

private class StringMapKeyValueIterator<T> {
	final h:Dynamic;
	final keys:Array<String>;
	final length:Int;
	var current:Int;

	public inline function new(h:Dynamic) {
		this.h = h;
		keys = Object.keys(h);
		length = keys.length;
		current = 0;
	}

	public inline function hasNext():Bool {
		return current < length;
	}

	public inline function next():{key:String, value:T} {
		var key = keys[current++];
		return {key: key, value: h[cast key]};
	}
}
#end
