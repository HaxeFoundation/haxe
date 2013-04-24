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
package haxe.ds;

abstract HashMap<K:{ function hashCode():Int; }, V >({keys:IntMap<K>, values:IntMap<V>}) {
	public function new() {
		this = { keys:new IntMap(), values: new IntMap() };
	}
	public inline function set(k:K, v:V) {
		this.keys.set(k.hashCode(), k);
		this.values.set(k.hashCode(), v);
	}
	public inline function get(k:K) {
		return this.values.get(k.hashCode());
	}
	public inline function exists(k:K) {
		return this.values.exists(k.hashCode());
	}
	public inline function remove(k:K) {
		this.values.remove(k.hashCode());
		return this.keys.remove(k.hashCode());
	}
	public inline function keys() {
		return this.keys.iterator();
	}
	public inline function iterator() {
		return this.values.iterator();
	}
	
	// TODO: this is required because abstracts do not unify against structures even
	// if they are structurally compatible
	@:to function toIMap():Map.IMap<K,V> {
		return untyped this;
	}
}