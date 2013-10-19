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
using Lambda;
abstract HashedStringMap<V>({keys:Array<String>, values:IntMap<V>}) {
	public function new() {
		this = { keys:[], values: new IntMap() };
	}
	public inline function set(k:String, v:V) {
		this.keys.push(k);
		this.values.set(hash(k), v);
	}
	public inline function get(k:String) {
		return this.values.get(hash(k));
	}
	public inline function exists(k:String) {
		return this.keys.has(k);
	}
	public inline function remove(k:String) {
		this.keys.remove(k);
		this.values.remove(hash(k));
	}
	public inline function keys() {
		return this.keys.iterator();
	}
	public inline function iterator() {
		return this.values.iterator();
	}
	static function hash(s:String):Int {
		var hash = 0;
		if (s.length == 0)
			return hash;
		for(i in 0...s.length) {
			var char = StringTools.fastCharCodeAt(s, i);
			hash = ((hash << 5) - hash) + char;
			hash = hash & hash; // Convert to 32bit integer
		}
		return hash;
	}
}