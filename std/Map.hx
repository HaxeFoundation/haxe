/*
 * Copyright (C)2005-2013 Haxe Foundation
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

 import haxe.ds.StringMap;
 import haxe.ds.IntMap;
 import haxe.ds.HashMap;
 import haxe.ds.ObjectMap;

@:multiType
abstract Map<K,V>(IMap < K, V > ) {
	public function new();

	@:to static inline function toStringMap(t:IMap < String, V > ):StringMap<V> {
		return new StringMap<V>();
	}

	@:to static inline function toIntMap(t:IMap < Int, V > ):IntMap<V> {
		return new IntMap<V>();
	}

	@:to static inline function toHashMap<K:Hashable>(t:IMap < K, V >):HashMap<K,V> {
		return new HashMap<K, V>();
	}
	
	@:to static inline function toObjectMap<K:{ }>(t:IMap < K, V >):ObjectMap<K,V> {
		return new ObjectMap<K, V>();
	}
	
	@:from static inline function fromStringMap<V>(map:StringMap<V>):Map<String,V> return map
	@:from static inline function fromIntMap<V>(map:IntMap<V>):Map<Int,V> return map
	@:from static inline function fromHashMap<K:Hashable,V>(map:HashMap<K,V>):Map<K,V> return map
	@:from static inline function fromObjectMap<K:{},V>(map:ObjectMap<K,V>):Map<K,V> return map

	public inline function set(k:K, v:V) this.set(k, v)
	public inline function get(k:K) return this.get(k)
	public inline function exists(k:K) return this.exists(k)
	public inline function remove(k:K) return this.remove(k)
	public inline function keys() return this.keys()
	public inline function iterator() return this.iterator()
}

private typedef IMap < K, V > = {
	public function get(k:K):V;
	public function set(k:K, v:V):Void;
	public function exists(k:K):Bool;
	public function remove(k:K):Bool;
	public function keys():Iterator<K>;
	public function iterator():Iterator<V>;
}

private typedef Hashable = {
	function hashCode():Int;
}