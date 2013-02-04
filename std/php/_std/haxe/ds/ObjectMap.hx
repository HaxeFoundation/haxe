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

@:coreApi
abstract ObjectMap <K:{ }, V>(StringMap<V>) {
	static function getId(key: { } ) {
		return untyped __php__("spl_object_hash($key)");
	}
	
	public function new(weakKeys:Bool = false) {
		this = new StringMap<V>();
	}
	
	public function set(key:K, value:V) untyped {
		untyped this.set(getId(key), value);
	}
	
	public function get(key:K) {
		return this.get(getId(key));
	}
	
	public function exists(key:K) {
		return this.exists(getId(key));
	}
	
	public function remove( key : K ) : Bool {
		return this.remove(getId(key));
	}
	
	public inline function keys() : Iterator<K> {
		return untyped __call__("new _hx_array_iterator", __call__("array_keys", this.h));
	}
	
	public inline function iterator() : Iterator<V> {
		return untyped __call__("new _hx_array_iterator", __call__("array_values", this.h));
	}
	
	public function toString() : String {
		var s = "{";
		var it = keys(this);
		for( i in it ) {
			s += i;
			s += " => ";
			s += Std.string(get(this,i));
			if( it.hasNext() )
				s += ", ";
		}
		return s + "}";
	}
}