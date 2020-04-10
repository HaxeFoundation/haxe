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
import js.lib.HaxeIterator;

// @:coreApi
class StringMap<V> extends NativeMap<String,V> implements IMap<String, V> {
	function keyIter():js.lib.Iterator<String> return super._keys();
	function valIter():js.lib.Iterator<V> return super._values();

	public inline function exists(k:String):Bool {
		return has(k);
	}

	public inline function remove(k:String):Bool {
		return delete(k);
	}

	public inline function keys():Iterator<String> {
		return new HaxeIterator(keyIter());
	}

	public inline function iterator():Iterator<V> {
		return new HaxeIterator(valIter());
	}

	public inline function keyValueIterator():KVIterator<String, V> {
		return new KVIterator(entries());
	}

	public inline function copy():IMap<String, V> {
		return js.Syntax.construct(StringMap, this);
	}

	public function toString():String {
		return null;
	}
}

@:native("Map")
private extern class NativeMap<K,V> {
	function new();
	@:pure function get(key:String):Null<V>;
	@:pure private function has(key:K):Bool;
	function set(key:K, value:V):Void;
	private function delete(key:K):Bool;
	function clear():Void;
	@:native("keys") private function _keys():js.lib.Iterator<K>;
	@:native("values") private function _values():js.lib.Iterator<V>;
	@:pure private function entries():js.lib.Iterator<js.lib.KeyValue<K, V>>;
}

private class KVIterator<K,V> {
	final jsIterator: js.lib.Iterator<js.lib.KeyValue<K,V>>;
	var lastStep: js.lib.Iterator.IteratorStep<js.lib.KeyValue<K,V>>;

	public inline function new(jsIterator: js.lib.Iterator<js.lib.KeyValue<K,V>>) {
		this.jsIterator = jsIterator;
		lastStep = jsIterator.next();
	}

	public inline function hasNext(): Bool {
		return !lastStep.done;
	}

	public inline function next(): {key:K, value:V} {
		var v = lastStep.value;
		lastStep = jsIterator.next();
		return {key: v.key, value: v.value};
	}
}

#elseif (js_es >= 5)
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
		return keysIterator(h);
	}

	public inline function iterator():Iterator<T> {
		return valueIterator(h);
	}

	public inline function keyValueIterator():KeyValueIterator<String, T> {
		return kvIterator(h);
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

	// impl

	static function keysIterator(h:Dynamic):Iterator<String> {
		var keys = Object.keys(h), len = keys.length, idx = 0;
		return {
			hasNext: () -> idx < len,
			next: () -> keys[idx++]
		};
	}

	static function valueIterator<T>(h:Dynamic):Iterator<T> {
		var keys = Object.keys(h), len = keys.length, idx = 0;
		return {
			hasNext: () -> idx < len,
			next: () -> h[cast keys[idx++]]
		};
	}

	static function kvIterator<T>(h:Dynamic):KeyValueIterator<String, T> {
		var keys = Object.keys(h), len = keys.length, idx = 0;
		return {
			hasNext: () -> idx < len,
			next: () -> {var k = keys[idx++]; {key: k, value: h[cast k]}}
		};
	}

	static function createCopy<T>(h:Dynamic):StringMap<T> {
		var copy = new StringMap();
		js.Syntax.code("for (var key in {0}) {1}[key] = {0}[key]", h, copy.h);
		return copy;
	}

	@:analyzer(no_optimize)
	static function stringify(h:Dynamic):String {
		var s = "{", first = true;
		js.Syntax.code("for (var key in {0}) {", h);
		js.Syntax.code("\tif ({0}) {0} = false; else {1} += ',';", first, s);
		js.Syntax.code("\t{0} += key + ' => ' + {1}({2}[key]);", s, Std.string, h);
		js.Syntax.code("}");
		return s + "}";
	}
}
#else
private class StringMapIterator<T> {
	var map:StringMap<T>;
	var keys:Array<String>;
	var index:Int;
	var count:Int;

	public inline function new(map:StringMap<T>, keys:Array<String>) {
		this.map = map;
		this.keys = keys;
		this.index = 0;
		this.count = keys.length;
	}

	public inline function hasNext() {
		return index < count;
	}

	public inline function next() {
		return map.get(keys[index++]);
	}
}

@:coreApi class StringMap<T> implements haxe.Constraints.IMap<String, T> {
	private var h:Dynamic;
	private var rh:Dynamic;

	public inline function new():Void {
		h = {};
	}

	inline function isReserved(key:String):Bool {
		return js.Syntax.code("__map_reserved[{0}]", key) != null;
	}

	public inline function set(key:String, value:T):Void {
		if (isReserved(key))
			setReserved(key, value);
		else
			h[cast key] = value;
	}

	public inline function get(key:String):Null<T> {
		if (isReserved(key))
			return getReserved(key);
		return h[cast key];
	}

	public inline function exists(key:String):Bool {
		if (isReserved(key))
			return existsReserved(key);
		return h.hasOwnProperty(key);
	}

	function setReserved(key:String, value:T):Void {
		if (rh == null)
			rh = {};
		rh[cast "$" + key] = value;
	}

	function getReserved(key:String):Null<T> {
		return rh == null ? null : rh[cast "$" + key];
	}

	function existsReserved(key:String):Bool {
		if (rh == null)
			return false;
		return (cast rh).hasOwnProperty("$" + key);
	}

	public function remove(key:String):Bool {
		if (isReserved(key)) {
			key = "$" + key;
			if (rh == null || !rh.hasOwnProperty(key))
				return false;
			js.Syntax.delete(rh, key);
			return true;
		} else {
			if (!h.hasOwnProperty(key))
				return false;
			js.Syntax.delete(h, key);
			return true;
		}
	}

	public function keys():Iterator<String> {
		return arrayKeys().iterator();
	}

	function arrayKeys():Array<String> {
		var out = [];
		untyped {
			js.Syntax.code("for( var key in this.h ) {");
			if (h.hasOwnProperty(key))
				out.push(key);
			js.Syntax.code("}");
		}
		if (rh != null)
			untyped {
				js.Syntax.code("for( var key in this.rh ) {");
				if (key.charCodeAt(0) == "$".code)
					out.push(key.substr(1));
				js.Syntax.code("}");
			}
		return out;
	}

	public inline function iterator():Iterator<T> {
		return new StringMapIterator(this, arrayKeys());
	}

	@:runtime public inline function keyValueIterator():KeyValueIterator<String, T> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	public function copy():StringMap<T> {
		var copied = new StringMap();
		for (key in keys())
			copied.set(key, get(key));
		return copied;
	}

	public function toString():String {
		var s = new StringBuf();
		s.add("{");
		var keys = arrayKeys();
		for (i in 0...keys.length) {
			var k = keys[i];
			s.add(k);
			s.add(" => ");
			s.add(Std.string(get(k)));
			if (i < keys.length - 1)
				s.add(", ");
		}
		s.add("}");
		return s.toString();
	}

	public inline function clear():Void {
		h = {};
		rh = null;
	}

	static function __init__():Void {
		js.Syntax.code("var __map_reserved = {};");
	}
}
#end
