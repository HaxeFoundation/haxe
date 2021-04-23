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

#if (js_es >= 6)
@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int, T> {
	public var size(get, never):Int;
	private var m:js.lib.Map<Int, T>;
	
	public inline function new(map = new js.lib.Map()):Void {
		m = map;
	}
	
	public inline function set(key:Int, value:T): Void {
		m.set(key, value);
	}
	
	public inline function get(key:Int):Null<T> {
		return m.get(key);
	}
	
	public inline function exists(key:Int):Bool {
		return m.has(key);
	}
	
	public inline function remove(key:Int):Bool {
		return m.delete(key);
	}
	
	public inline function keys():Iterator<Int> {
		return new js.lib.HaxeIterator(m.keys());
	}
	
	public inline function iterator():Iterator<T> {
		return m.iterator();
	}
	
	public inline function keyValueIterator():KeyValueIterator<Int, T> {
		return m.keyValueIterator();
	}
	
	public inline function copy():IntMap<T> {
		return new IntMap(new js.lib.Map(m));
	}
	
	public function toString():String {
		var s = new StringBuf();
		s.add("{");
		var it = keyValueIterator();
		for (i in it) {
			s.add(i.key);
			s.add(" => ");
			s.add(Std.string(i.value));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}
	
	public inline function clear():Void {
		m.clear();
	}
	
	private inline function get_size():Int {
		return m.size;
	}
}

#else
@:coreApi class IntMap<T> implements haxe.Constraints.IMap<Int, T> {
	public var size(get, never):Int;
	private var h:Dynamic;

	public inline function new():Void {
		h = {};
	}

	public inline function set(key:Int, value:T):Void {
		h[key] = value;
	}

	public inline function get(key:Int):Null<T> {
		return h[key];
	}

	public inline function exists(key:Int):Bool {
		return (cast h).hasOwnProperty(key);
	}

	public function remove(key:Int):Bool {
		if (!(cast h).hasOwnProperty(key))
			return false;
		js.Syntax.delete(h, key);
		return true;
	}

	public function keys():Iterator<Int> {
		var a = [];
		js.Syntax.code("for( var key in {0} ) if({0}.hasOwnProperty(key)) {1}.push(key | 0)", h, a);
		return a.iterator();
	}

	public function iterator():Iterator<T> {
		return untyped {
			ref: h,
			it: keys(),
			hasNext: function() {
				return __this__.it.hasNext();
			},
			next: function() {
				var i = __this__.it.next();
				return __this__.ref[i];
			}
		};
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<Int, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():IntMap<T> {
		var copied = new IntMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for (i in it) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	public inline function clear():Void {
		h = {};
	}
	
	private inline function get_size():Int {
		var s = 0;
		js.Syntax.code("for( var key in {0} ) if({0}.hasOwnProperty(key)) {1}++", h, s);
		return s;
	}
}
#end
