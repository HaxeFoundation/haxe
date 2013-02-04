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

@:multiType
abstract Map(IMap < K, V > )<K,V> {
	public function new();

	@:to static inline function toHash(t:IMap < String, V > ):haxe.ds.StringMap<V> {
		return new haxe.ds.StringMap<V>();
	}

	@:to static inline function toIntHash(t:IMap < Int, V > ):haxe.ds.IntMap<V> {
		return new haxe.ds.IntMap<V>();
	}

	@:to static inline function toHashMap<K:{ function hashCode():Int; }>(t:IMap < K, V >):haxe.ds.HashMap<K,V> {
		return new haxe.ds.HashMap<K, V>();
	}
	
	@:to static inline function toObjectMap<K:{ }>(t:IMap < K, V >):haxe.ds.ObjectMap<K,V> {
		return new haxe.ds.ObjectMap<K, V>();
	}

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
