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

package haxe.ds;

@:coreApi
class IntMap<T> implements haxe.Constraints.IMap<Int,T> {

	var h : hl.types.IntMap;

	public function new() : Void {
		h = new hl.types.IntMap();
	}

	public function set( key : Int, value : T ) : Void {
		@:privateAccess h.set(key,value);
	}

	public function get( key : Int ) : Null<T> {
		return @:privateAccess h.get(key);
	}

	public function exists( key : Int ) : Bool {
		return @:privateAccess h.exists(key);
	}

	public function remove( key : Int ) : Bool {
		return @:privateAccess h.remove(key);
	}

	public function keys() : Iterator<Int> {
		return new hl.NativeArray.NativeArrayIterator<Int>(h.keysArray());
	}

	public function iterator() : Iterator<T> {
		return h.iterator();
	}
	
	public function copy() : IntMap<T> {
		var copied = new IntMap();
		for(key in keys()) copied.set(key, get(key));
		return copied;
	}

	public function toString() : String {
		var s = new StringBuf();
		var keys = h.keysArray();
		var values = h.valuesArray();
		s.addChar('{'.code);
		for( i in 0...keys.length ) {
			if( i > 0 )
				s.add(", ");
			s.add(keys[i]);
			s.add(" => ");
			s.add(values[i]);
		}
		s.addChar('}'.code);
		return s.toString();
	}

}
