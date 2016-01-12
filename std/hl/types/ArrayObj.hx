package hl.types;

@:keep
class ArrayObj<T> extends ArrayBase {

	var array : hl.types.NativeArray<Dynamic>;

	public function new() {
		length = 0;
		array = new NativeArray<Dynamic>(0);
	}

	public function concat( a : ArrayObj<T> ) : ArrayObj<T> {
		throw "TODO";
		return null;
	}

	override function join( sep : String ) : String {
		var b = new StringBuf();
		for( i in 0...length ) {
			if( i > 0 ) b.add(sep);
			b.add(array[i]);
		}
		return b.toString();
	}

	public function pop() : Null<T> {
		if( length == 0 )
			return null;
		length--;
		var v = array[length];
		array[length] = null;
		return v;
	}

	public function push(x : T) : Int {
		var len = length;
		if( array.length == len )
			__expand(len);
		else
			length++;
		array[len] = x;
		return length;
	}

	override function reverse() : Void {
		throw "TODO";
	}

	public function shift() : Null<T> {
		throw "TODO";
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : ArrayObj<T> {
		throw "TODO";
		return null;
	}

	public function sort( f : T -> T -> Int ) : Void {
		// TODO : use native call ?
		haxe.ds.ArraySort.sort(cast toDynamic(), f);
	}

	public function splice( pos : Int, len : Int ) : ArrayObj<T> {
		if( len < 0 ) return new ArrayObj();
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
		var a = this.array;
		var ret : ArrayObj<T> = alloc(cast a.sub(pos,len));
		var end = pos + len;
		a.blit(pos,a,end,this.length-end);
		this.length -= len;
		while( --len >= 0 )
			a[this.length + len] = null;
		return ret;
	}

	override function toString() : String {
		var b = new StringBuf();
		b.addChar("[".code);
		for( i in 0...length ) {
			if( i > 0 ) b.addChar(",".code);
			b.add(array[i]);
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
		var i = indexOf(x);
		if( i < 0 ) return false;
		length--;
		array.blit(i,array,i+1,length - i);
		array[length] = null;
		return true;
	}

	public function indexOf( x : T, ?fromIndex:Int ) : Int {
		var i : Int = fromIndex;
		var length = length;
		var array = array;
		while( i < length ) {
			if( array[i] == x )
				return i;
			i++;
		}
		return -1;
	}

	public function lastIndexOf( x : T, ?fromIndex:Int ) : Int {
		throw "TODO";
		return -1;
	}

	public function copy() : ArrayObj<T> {
		throw "TODO";
		return null;
	}

	public function iterator() : Iterator<T> {
		throw "TODO";
		return null;
	}

	public function map<S>( f : T -> S ) : ArrayObj<S> {
		throw "TODO";
		return null;
	}

	public function filter( f : T -> Bool ) : ArrayObj<T> {
		throw "TODO";
		return null;
	}

	// called by compiler when accessing the array outside of its bounds, might trigger resize
	function __expand( index : Int ) {
		if( index < 0 ) throw "Invalid array access";
		var newlen = index + 1;
		var size : Int = array.length;
		if( newlen > size ) {
			var next = (size * 3) >> 1;
			if( next < newlen ) next = newlen;
			var arr2 = new hl.types.NativeArray<Dynamic>(next);
			arr2.blit(0,array,0,length);
			array = arr2;
		}
		length = newlen;
	}

	override function getDyn( pos : Int ) : Dynamic {
		var pos : UInt = pos;
		if( pos >= length )
			return null;
		return array[pos];
	}

	override function setDyn( pos : Int, v : Dynamic ) {
		var pos : UInt = pos;
		if( pos >= length )
			__expand(pos);
		array[pos] = Api.safeCast(v,array.getType());
	}

	override function toDynamic() : ArrayDyn {
		return ArrayDyn.alloc(this, false);
	}
	override function pushDyn( v : Dynamic ) return push(v);
	override function popDyn() : Null<Dynamic> return pop();
	override function shiftDyn() : Null<Dynamic> return shift();
	override function unshiftDyn( v : Dynamic ) unshift(v);
	override function insertDyn( pos : Int, v : Dynamic ) insert(pos, v);
	override function removeDyn( v : Dynamic ) return remove(v);
	override function sortDyn( f : Dynamic -> Dynamic -> Int ) sort(f);
	override function spliceDyn( pos : Int, len : Int ) return splice(pos, len);

	public static function alloc<T>( a : hl.types.NativeArray<T> ) : ArrayObj<T> {
		var arr : ArrayObj<T> = untyped $new(ArrayObj);
		arr.array = a;
		arr.length = a.length;
		return arr;
	}

}
