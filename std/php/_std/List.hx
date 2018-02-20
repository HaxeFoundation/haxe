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
@:coreApi class List<T> implements php.IteratorAggregate<T> {

	@:ifFeature("List.iterator") private var h : ArrayAccess<Dynamic>;
	@:ifFeature("List.iterator") private var q : ArrayAccess<Dynamic>;

	public var length(default,null) : Int;

	public function new() : Void {
		length = 0;
	}

	public function add( item : T ) : Void {
		var x = untyped __call__('array', item, null);
		if( h == null )
			untyped __php__("$this->h =& $x");
		else
			untyped __php__("$this->q[1] =& $x");
		untyped __php__("$this->q =& $x");
		length++;
	}

	public function push( item : T ) : Void {
		var x = untyped __call__('array', item, __php__("&$this->h"));
		untyped __php__("$this->h =& $x");
		if( q == null )
			untyped __php__("$this->q =& $x");
		length++;
	}

	public function first() : Null<T> {
		return if( h == null ) null else h[0];
	}

	public function last() : Null<T> {
		return if( q == null ) null else q[0];
	}

	public function pop() : Null<T> {
		if( h == null )
			return null;
		var x = h[0];
		untyped __php__("$this->h =& $this->h[1]");
		if( h == null )
			q = null;
		length--;
		return x;
	}

	public function isEmpty() : Bool {
		return (h == null);
	}

	public function clear() : Void {
		h = null;
		q = null;
		length = 0;
	}

	public function remove( v : T ) : Bool {
		var prev = null;
		var l = untyped __php__("& $this->h");
		while( l != null ) {
			if(untyped __php__("$l[0] === $v")) {
				if( prev == null )
					untyped __php__("$this->h =& $l[1]");
				else
					untyped __php__("$prev[1] =& $l[1]");
				if(untyped __physeq__(q, l))
					untyped __php__("$this->q =& $prev");
				length--;
				return true;
			}
			untyped __php__("$prev =& $l");
			untyped __php__("$l =& $l[1]");
		}
		return false;
	}

	public function iterator() : ListIterator<T> {
		return untyped __call__("new _hx_list_iterator", this);
	}

	public function toString() : String {
		var s = "";
		var first = true;
		var l = h;
		while( l != null ) {
			if( first )
				first = false;
			else
				s += ", ";
			s += Std.string(l[0]);
			l = l[1];
		}
		return "{" + s + "}";
	}

	public function join(sep : String) : String {
		var s = "";
		var first = true;
		var l = h;
		while( l != null ) {
			if( first )
				first = false;
			else
				s += sep;
			s += l[0];
			l = l[1];
		}
		return s;
	}

	public function filter( f : T -> Bool ) : List<T> {
		var l2 = new List();
		var l = h;
		while( l != null ) {
			var v = l[0];
			l = l[1];
			if( f(v) )
				l2.add(v);
		}
		return l2;
	}

	public function map<X>(f : T -> X) : List<X> {
		var b = new List();
		var l = h;
		while( l != null ) {
			var v = l[0];
			l = l[1];
			b.add(f(v));
		}
		return b;
	}

	function getIterator() : Iterator<T> {
		return iterator();
	}
}

@:coreType private extern class ListIterator<T> {
	function hasNext():Bool;
	function next():T;
}
