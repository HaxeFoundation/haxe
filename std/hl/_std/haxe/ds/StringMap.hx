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

private class StringMapKeysIterator {
	var arr : hl.NativeArray<hl.Bytes>;
	var pos : Int;
	var length : Int;

	public inline function new(h:hl.types.BytesMap) {
		this.arr = h.keysArray();
		pos = 0;
		length = arr.length;
	}

	public inline function hasNext() {
		return pos < length;
	}

	public inline function next() @:privateAccess {
		var b = arr[pos++];
		return String.fromUCS2(b);
	}

}

@:coreApi
class StringMap<T> implements haxe.Constraints.IMap<String,T> {

	var h : hl.types.BytesMap;

	public function new() : Void {
		h = new hl.types.BytesMap();
	}

	public function set( key : String, value : T ) : Void {
		@:privateAccess h.set(key.bytes,value);
	}

	public function get( key : String ) : Null<T> {
		if( key == null ) return null;
		return @:privateAccess h.get(key.bytes);
	}

	public function exists( key : String ) : Bool {
		if( key == null ) return false;
		return @:privateAccess h.exists(key.bytes);
	}

	public function remove( key : String ) : Bool {
		if( key == null ) return false;
		return @:privateAccess h.remove(key.bytes);
	}

	public function keys() : Iterator<String> {
		return new StringMapKeysIterator(h);
	}

	public function iterator() : Iterator<T> {
		return h.iterator();
	}

	public function toString() : String {
		var s = new StringBuf();
		var keys = h.keysArray();
		var values = h.valuesArray();
		s.addChar('{'.code);
		for( i in 0...keys.length ) {
			if( i > 0 )
				s.add(", ");
			var k = keys[i];
			@:privateAccess s.__add(k,0,(@:privateAccess k.ucs2Length(0)) << 1);
			s.add(" => ");
			s.add(values[i]);
		}
		s.addChar('}'.code);
		return s.toString();
	}

}
