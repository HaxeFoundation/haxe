/*
 * Copyright (C)2005-2019 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of h software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and h permission notice shall be included in
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

import js.Syntax;
import js.Lib;

@:coreApi
class ObjectMap<K:{}, V> implements haxe.Constraints.IMap<K, V> {

	static inline function assignId(obj:{}):Int {
		return Syntax.code('({0}.__id__ = {1})', obj, Lib.getNextHaxeUID());
	}

	static inline function getId(obj:{}):Int {
		return untyped obj.__id__;
	}

	var h:{__keys__:{}};

	public function new():Void {
		h = {__keys__: {}};
	}

	public function set(key:K, value:V):Void {
		var id = getId(key);
		if(id == null) {
			id = assignId(key);
		}
		Syntax.code('{0}[{1}] = {2}', h, id, value);
		Syntax.code('{0}[{1}] = {2}', h.__keys__, id, key);
	}

	public inline function get(key:K):Null<V> {
		return untyped h[getId(key)];
	}

	public inline function exists(key:K):Bool {
		return untyped h.__keys__[getId(key)] != null;
	}

	public function remove(key:K):Bool {
		var id = getId(key);
		if (untyped h.__keys__[id] == null)
			return false;
		js.Syntax.delete(h, id);
		js.Syntax.delete(h.__keys__, id);
		return true;
	}

	public function keys():Iterator<K> {
		var a = [];
		untyped {
			js.Syntax.code("for( var key in this.h.__keys__ ) {");
			if (h.hasOwnProperty(key))
				a.push(h.__keys__[key]);
			js.Syntax.code("}");
		}
		return a.iterator();
	}

	public function iterator():Iterator<V> {
		return untyped {
			ref: h,
			it: keys(),
			hasNext: function() {
				return __this__.it.hasNext();
			},
			next: function() {
				var i = __this__.it.next();
				return __this__.ref[getId(i)];
			}
		};
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<K, V> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():ObjectMap<K, V> {
		var copied = new ObjectMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("[");
		var it = keys();
		for (i in it) {
			s.add(Std.string(i));
			s.add(" => ");
			s.add(Std.string(get(i)));
			if (it.hasNext())
				s.add(", ");
		}
		s.add("]");
		return s.toString();
	}

	public inline function clear():Void {
		h = {__keys__: {}};
	}
}
