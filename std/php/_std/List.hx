/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

@:coreApi class List<T> implements php.IteratorAggregate<T> {

	private var h : ArrayAccess<Dynamic>;
	private var q : ArrayAccess<Dynamic>;

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
		h = h[1];
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

	public function iterator() : Iterator<T> {
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
