/*
 * Copyright (C)2005-2016 Haxe Foundation
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
class ArrayAccess {

	public function getDyn( pos : Int ) : Dynamic {
		throw "Not implemented";
		return 0;
	}

	public function setDyn( pos : Int, v : Dynamic ) {
		throw "Not implemented";
	}

	public function blit( pos : Int, src : ArrayAccess, srcpos : Int, len : Int ) : Void {
		throw "Not implemented";		
	}

}

@:keep
class ArrayBase extends ArrayAccess {

	public var length(default,null) : Int;

	public function pushDyn( v : Dynamic ) : Int {
		throw "Not implemented";
		return 0;
	}

	public function popDyn() : Null<Dynamic> {
		throw "Not implemented";
		return null;
	}

	public function shiftDyn() : Null<Dynamic> {
		throw "Not implemented";
		return null;
	}

	public function unshiftDyn( v : Dynamic ) : Void {
		throw "Not implemented";
	}

	public function insertDyn( pos : Int, v : Dynamic ) : Void {
		throw "Not implemented";
	}

	public function removeDyn( v : Dynamic ) : Bool {
		throw "Not implemented";
		return false;
	}

	public function sortDyn( f : Dynamic -> Dynamic -> Int ) : Void {
		throw "Not implemented";
	}

	public function slice( pos : Int, ?end : Int ) : ArrayBase{
		throw "Not implemented";
		return null;
	}

	public function splice( pos : Int, len : Int ) : ArrayBase{
		throw "Not implemented";
		return null;
	}

	public function join( sep : String ) : String {
		throw "Not implemented";
		return null;
	}

	public function reverse() {
		throw "Not implemented";
	}

	public function toString() : String {
		throw "Not implemented";
		return null;
	}

	function __cast( t : Type ) : Dynamic {
		if( t == Type.get(new ArrayDyn()) )
			return ArrayDyn.alloc(this, false);
		return null;
	}

	public static function allocI32( bytes : BytesAccess<Int>, length : Int ) @:privateAccess {
		var a : ArrayI32 = untyped $new(ArrayI32);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocUI16( bytes : BytesAccess<UI16>, length : Int ) @:privateAccess {
		var a : ArrayUI16 = untyped $new(ArrayUI16);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocF32( bytes : BytesAccess<F32>, length : Int ) @:privateAccess {
		var a : ArrayF32 = untyped $new(ArrayF32);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

	public static function allocF64( bytes : BytesAccess<Float>, length : Int ) @:privateAccess {
		var a : ArrayF64 = untyped $new(ArrayF64);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

}

@:keep
@:generic
class BasicIterator<T> {
	var pos : Int;
	var a : ArrayBasic<T>;
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
@:generic class ArrayBasic<T> extends ArrayBase {

	var bytes : hl.types.BytesAccess<T>;
	var size : Int;

	public function new() {
		size = length = 0;
		bytes = new Bytes(0);
	}

	public function concat( a : ArrayBasic<T> ) : ArrayBasic<T> {
		var ac = new ArrayBasic<T>();
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
	
	override function blit( pos : Int, src : ArrayAccess, srcpos : Int, len : Int ) : Void {
		var src = (cast src : ArrayBasic<T>);
		if( pos < 0 || srcpos < 0 || len < 0 || pos + len > length || srcpos + len > src.length ) throw haxe.io.Error.OutsideBounds;
		(bytes:Bytes).blit(pos << bytes.sizeBits,src.bytes,srcpos<<bytes.sizeBits,len<<bytes.sizeBits);
	}

	override function slice( pos : Int, ?end : Int ) : ArrayBasic<T> {
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
			return new ArrayBasic<T>();
		var a = new ArrayBasic<T>();
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

	override function splice( pos : Int, len : Int ) : ArrayBasic<T> {
		throw "TODO";
		return null;
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
		(bytes : hl.types.Bytes).blit(idx<<bytes.sizeBits,bytes,(idx + 1)<<bytes.sizeBits,(length - idx)<<bytes.sizeBits);
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

	public function copy() : ArrayBasic<T> {
		var a = new ArrayBasic<T>();
		a.length = a.size = length;
		a.bytes = new Bytes(length << bytes.sizeBits);
		(a.bytes:Bytes).blit(0, bytes, 0, length << bytes.sizeBits);
		return a;
	}

	public function iterator() : Iterator<T> {
		return new BasicIterator(this);
	}

	public function map<S>( f : T -> S ) : ArrayDyn @:privateAccess {
		var a = new ArrayObj();
		if( length > 0 ) a.__expand(length - 1);
		for( i in 0...length )
			a.array[i] = f(bytes[i]);
		return ArrayDyn.alloc(a,true);
	}

	public function filter( f : T -> Bool ) : ArrayBasic<T> {
		var a = new ArrayBasic<T>();
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
		if( index < 0 ) throw "Invalid array access";
		var newlen = index + 1;
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var bytes2 = new hl.types.Bytes(next << bytes.sizeBits);
			var bsize = length << bytes.sizeBits;
			bytes2.blit(0, bytes, 0, bsize);
			bytes2.fill(bsize, (next << bytes.sizeBits) - bsize, 0);
			bytes = bytes2;
			size = next;
		}
		length = newlen;
	}

}

typedef ArrayI32 = ArrayBasic<Int>;
typedef ArrayUI16 = ArrayBasic<UI16>;
typedef ArrayF32 = ArrayBasic<F32>;
typedef ArrayF64 = ArrayBasic<Float>;
