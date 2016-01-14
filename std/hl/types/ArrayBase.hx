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

	public function spliceDyn( pos : Int, len : Int ) : ArrayObj<Dynamic> {
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

	public static function allocF64( bytes : BytesAccess<Float>, length : Int ) @:privateAccess {
		var a : ArrayF64 = untyped $new(ArrayF64);
		a.length = length;
		a.bytes = bytes;
		a.size = length;
		return a;
	}

}

@:generic class ArrayBasic<T> extends ArrayBase {

	var bytes : hl.types.BytesAccess<T>;
	var size : Int;

	public function new() {
		size = length = 0;
		bytes = new Bytes(0);
	}

	public function concat( a : ArrayBasic<T> ) : ArrayBasic<T> {
		throw "TODO";
		return null;
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
		throw "TODO";
	}

	public function shift() : Null<T> {
		throw "TODO";
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : ArrayBasic<T> {
		throw "TODO";
		return null;
	}

	public function sort( f : T -> T -> Int ) : Void {
		trace("TODO");
	}

	public function splice( pos : Int, len : Int ) : ArrayBasic<T> {
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
		throw "TODO";
	}

	public function insert( pos : Int, x : T ) : Void {
		throw "TODO";
	}

	public function remove( x : T ) : Bool {
		throw "TODO";
		return false;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function copy() : ArrayBasic<T> {
		throw "TODO";
		return null;
	}

	public function iterator() : Iterator<T> {
		throw "TODO";
		return null;
	}

	public function map<S>( f : T -> S ) : ArrayDyn {
		throw "TODO";
		return null;
	}

	public function filter( f : Int -> Bool ) : ArrayBasic<T> {
		throw "TODO";
		return null;
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
	override function spliceDyn( pos : Int, len : Int ) : ArrayObj<Dynamic> {
		throw "Not implemented";
		return null;
	}

	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand( index : Int ) {
		if( index < 0 ) throw "Invalid array access";
		var newlen = index + 1;
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var bytes2 = new hl.types.Bytes(next << bytes.sizeBits);
			bytes2.blit(0,bytes,0,length << bytes.sizeBits);
			bytes = bytes2;
			size = next;
		}
		length = newlen;
	}

}

typedef ArrayI32 = ArrayBasic<Int>;
typedef ArrayF64 = ArrayBasic<Float>;
