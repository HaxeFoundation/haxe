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

class List<T> {

	private var h : Array<Dynamic>;
	private var q : Array<Dynamic>;
	public property length(default,null) : Int;

	public function new() {
		length = 0;
	}

	public function add( item : T ) {
		var x = #if neko
			untyped __dollar__array(item,null)
		#else true
			[item,null]
		#end;
		if( h == null )
			h = x;
		else
			q[1] = x;
		q = x;
		length++;
	}

	public function push( item : T ) {
		var x = #if neko
			untyped __dollar__array(item,q)
		#else true
			[item,q]
		#end;
		h = x;
		if( q == null )
			q = x;
		length++;
	}

	public function first() : T {
		return if( h == null ) null else h[0];
	}

	public function last() : T {
		return if( q == null ) null else q[0];
	}

	public function pop() : T {
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
		return (h != null);
	}

	public function remove( v : T ) : Bool {
		var prev = null;
		var l = h;
		while( l != null ) {
			if( l[0] == v ) {
				if( prev == null )
					h = l[1];
				else
					prev[1] = l[1];
				if( q == l )
					q = prev;
				length--;
				return true;
			}
			prev = l;
			l = l[1];
		}
		return false;
	}

	public function iterator() : Iterator<T> {
		return {
			h : h,
			hasNext : function() {
				return untyped (this.h != null);
			},
			next : function() {
				untyped {
					if( this.h == null )
						return null;
					var x = this.h[0];
					this.h = this.h[1];
					return x;
				}
			}
		}
	}

	public function toString() {
		var s = new StringBuf();
		var first = true;
		var l = h;
		s.add("{");
		while( l != null ) {
			if( first )
				first = false;
			else
				s.add(", ");
			s.add(l[0]);
			l = l[1];
		}
		s.add("}");
		return s.toString();
	}

	public function join(sep : String) {
		var s = new StringBuf();
		var first = true;
		var l = h;
		while( l != null ) {
			if( first )
				first = false;
			else
				s.add(sep);
			s.add(l[0]);
			l = l[1];
		}
		return s.toString();
	}

	public function filter( f : T -> Bool ) {
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

}
