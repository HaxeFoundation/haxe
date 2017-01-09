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
package hl.types;

@:keep
@:generic
class BytesIterator<T> {
	var pos : Int;
	var a : ArrayBytes<T>;
	public function new(a) {
		this.a = a;
	}
	public function hasNext() {
		return pos < a.length;
	}
	public function next() : T {
		return @:privateAccess a.bytes.get(pos++);
	}
}

@:keep
@:generic class ArrayBytes<T> extends ArrayBase {

	var bytes : hl.BytesAccess<T>;
	var size : Int;

	public function new() {
		size = length = 0;
		bytes = null;
	}

	public function concat( a : ArrayBytes<T> ) : ArrayBytes<T> {
		var ac = new ArrayBytes<T>();
		ac.length = ac.size = length + a.length;
		ac.bytes = new Bytes(ac.length << bytes.sizeBits);
		var offset = length << bytes.sizeBits;
		(ac.bytes:Bytes).blit(0, this.bytes, 0, offset);
		(ac.bytes:Bytes).blit(offset, a.bytes, 0, a.length << bytes.sizeBits);
		return ac;
	}

	override function join( sep : String ) : String {
		var s = new StringBuf();
		for( i in 0...length ) {
			if( i > 0 ) s.add(sep);
			s.add(bytes[i]);
		}
		return s.toString();
	}

	public function pop() : Null<T> {
		if( length == 0 )
			return null;
		length--;
		return bytes[length];
	}

	public function push(x : T) : Int {
		var len = length;
		if( size == len )
			__expand(len);
		else
			length++;
		bytes[len] = x;
		return length;
	}

	override function reverse() : Void {
		for( i in 0...length >> 1 ) {
			var k = length - 1 - i;
			var tmp = bytes[i];
			bytes[i] = bytes[k];
			bytes[k] = tmp;
		}
	}

	public function shift() : Null<T> {
		if( length == 0 )
			return null;
		var v = bytes[0];
		length--;
		(bytes:Bytes).blit(0, bytes, 1 << bytes.sizeBits, length << bytes.sizeBits);
		return v;
	}

	override function blit( pos : Int, src : ArrayBase.ArrayAccess, srcpos : Int, len : Int ) : Void {
		var src = (cast src : ArrayBytes<T>);
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length ) throw haxe.io.Error.OutsideBounds;
		(bytes:Bytes).blit(pos << bytes.sizeBits,src.bytes,srcpos<<bytes.sizeBits,len<<bytes.sizeBits);
	}

	override function slice( pos : Int, ?end : Int ) : ArrayBytes<T> {
		if( pos < 0 ) {
			pos = this.length + pos;
			if( pos < 0 )
				pos = 0;
		}
		var pend : Int;
		if( end == null )
			pend = this.length;
		else {
			pend = end;
			if( pend < 0 )
				pend += this.length;
			if( pend > this.length )
				pend = this.length;
		}
		var len = pend - pos;
		if( len < 0 )
			return new ArrayBytes<T>();
		var a = new ArrayBytes<T>();
		a.length = a.size = len;
		a.bytes = (bytes:Bytes).sub(pos << bytes.sizeBits, len << bytes.sizeBits);
		return a;
	}

	public function sort( f : T -> T -> Int ) : Void {
		if( Type.get((cast null : T)) == Type.get(0) )
			(bytes:Bytes).sortI32(0, length, cast f);
		else
			(bytes:Bytes).sortF64(0, length, cast f);
	}

	override function splice( pos : Int, len : Int ) : ArrayBytes<T> {
		if( len < 0 )
			return new ArrayBytes<T>();
		if( pos < 0 ){
			pos = this.length + pos;
			if( pos < 0 ) pos = 0;
		}
		if( pos > this.length ) {
			pos = 0;
			len = 0;
		} else if( pos + len > this.length ) {
			len = this.length - pos;
			if( len < 0 ) len = 0;
		}
		if( len == 0 )
			return new ArrayBytes<T>();
		var ret = new ArrayBytes<T>();
		ret.bytes = (bytes:Bytes).sub(pos << bytes.sizeBits, len << bytes.sizeBits);
		ret.size = ret.length = len;
		var end = pos + len;
		(bytes:Bytes).blit(pos << bytes.sizeBits, bytes, end << bytes.sizeBits, (length - end) << bytes.sizeBits);
		length -= len;
		return ret;
	}

	override function toString() : String {
		var b = new StringBuf();
		b.addChar("[".code);
		for( i in 0...length ) {
			if( i > 0 ) b.addChar(",".code);
			b.add(bytes[i]);
		}
		b.addChar("]".code);
		return b.toString();
	}

	public function unshift( x : T ) : Void {
		if( length == size ) __expand(length) else length++;
		(bytes:Bytes).blit(1<<bytes.sizeBits, bytes, 0, (length - 1) << bytes.sizeBits);
		bytes[0] = x;
	}

	public function insert( pos : Int, x : T ) : Void {
		if( pos < 0 ) {
			pos = length + pos;
			if( pos < 0 ) pos = 0;
		} else if( pos > length ) pos = length;
		if( length == size ) __expand(length) else length++;
		(bytes:Bytes).blit((pos + 1)<<bytes.sizeBits, bytes, pos<<bytes.sizeBits, (length - pos - 1) << bytes.sizeBits);
		bytes[pos] = x;
	}

	public function remove( x : T ) : Bool {
		var idx = indexOf(x);
		if( idx < 0 )
			return false;
		length--;
		(bytes : hl.Bytes).blit(idx<<bytes.sizeBits,bytes,(idx + 1)<<bytes.sizeBits,(length - idx)<<bytes.sizeBits);
		return true;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int {
		var idx : Int = fromIndex == null ? 0 : fromIndex;
		if( idx < 0 ) {
			idx += length;
			if( idx < 0 ) idx = 0;
		}
		for( i in idx...length )
			if( bytes[i] == x )
				return i;
		return -1;
	}

	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int {
		var len = length;
		var i:Int = fromIndex != null ? fromIndex : len - 1;
		if( i >= len )
			i = len - 1;
		else if( i  < 0 )
			i += len;
		while( i >= 0 ) {
			if( bytes[i] == x )
				return i;
			i--;
		}
		return -1;
	}

	public function copy() : ArrayBytes<T> {
		var a = new ArrayBytes<T>();
		a.length = a.size = length;
		a.bytes = new Bytes(length << bytes.sizeBits);
		(a.bytes:Bytes).blit(0, bytes, 0, length << bytes.sizeBits);
		return a;
	}

	public function iterator() : Iterator<T> {
		return new BytesIterator(this);
	}

	public function map<S>( f : T -> S ) : ArrayDyn @:privateAccess {
		var a = new ArrayObj();
		if( length > 0 ) a.__expand(length - 1);
		for( i in 0...length )
			a.array[i] = f(bytes[i]);
		return ArrayDyn.alloc(a,true);
	}

	public function filter( f : T -> Bool ) : ArrayBytes<T> {
		var a = new ArrayBytes<T>();
		for( i in 0...length ) {
			var v = bytes[i];
			if( f(v) ) a.push(v);
		}
		return a;
	}

	override function getDyn( pos : Int ) : Dynamic {
		var pos : UInt = pos;
		if( pos >= length )
			return bytes.nullValue;
		return bytes[pos];
	}

	override function setDyn( pos : Int, v : Dynamic ) {
		var pos : UInt = pos;
		if( pos >= length )
			__expand(pos);
		bytes[pos] = v;
	}

	override function pushDyn( v : Dynamic ) return push(v);
	override function popDyn() : Null<Dynamic> return pop();
	override function shiftDyn() : Null<Dynamic> return shift();
	override function unshiftDyn( v : Dynamic ) unshift(v);
	override function insertDyn( pos : Int, v : Dynamic ) insert(pos, v);
	override function removeDyn( v : Dynamic ) return remove(v);
	override function sortDyn( f : Dynamic -> Dynamic -> Int ) sort(f);

	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand( index : Int ) {
		if( index < 0 ) throw "Invalid array index "+index;
		var newlen = index + 1;
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var bytes2 = new hl.Bytes(next << bytes.sizeBits);
			var bsize = length << bytes.sizeBits;
			bytes2.blit(0, bytes, 0, bsize);
			bytes2.fill(bsize, (next << bytes.sizeBits) - bsize, 0);
			bytes = bytes2;
			size = next;
		}
		length = newlen;
	}

}

typedef ArrayI32 = ArrayBytes<Int>;
typedef ArrayUI16 = ArrayBytes<UI16>;
typedef ArrayF32 = ArrayBytes<F32>;
typedef ArrayF64 = ArrayBytes<Float>;
