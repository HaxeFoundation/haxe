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

/**
	A linked-list of elements. The list is composed of two-elements arrays
	that are chained together. It's optimized so that adding or removing an
	element doesn't imply to copy the whole array content everytime.
**/
class List<T> {

	#if php
	private var h : ArrayAccess<Dynamic>;
	private var q : ArrayAccess<Dynamic>;
	#else
	private var h : Array<Dynamic>;
	private var q : Array<Dynamic>;
	#end

	/**
		The number of elements in this list.
	**/
	public var length(default,null) : Int;

	/**
		Creates a new empty list.
	**/
	public function new() {
		length = 0;
	}

	/**
		Add an element at the end of the list.
	**/
	public function add( item : T ) {
		var x = #if neko untyped __dollar__array(item,null) #elseif php untyped __call__('array', item, null) #else [item] #end;
		if( h == null )
		#if php
			untyped __php__("$this->h =& $x");
		else
			untyped __php__("$this->q[1] =& $x");
		untyped __php__("$this->q =& $x");
		#else
			h = x;
		else
			q[1] = x;
		q = x;
		#end
		length++;
	}

	/**
		Push an element at the beginning of the list.
	**/
	public function push( item : T ) {
		var x = #if neko
			untyped __dollar__array(item,h)
		#elseif php
			untyped __call__('array', item, __php__("&$this->h"))
		#else
			[item,h]
		#end;
		#if php
		untyped __php__("$this->h =& $x");
		if( q == null )
			untyped __php__("$this->q =& $x");		
		#else
		h = x;
		if( q == null )
			q = x;
		#end
		length++;
	}

	/**
		Returns the first element of the list, or null
		if the list is empty.
	**/
	public function first() : T {
		return if( h == null ) null else h[0];
	}

	/**
		Returns the last element of the list, or null
		if the list is empty.
	**/
	public function last() : T {
		return if( q == null ) null else q[0];
	}


	/**
		Removes the first element of the list and
		returns it or simply returns null if the
		list is empty.
	**/
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

	/**
		Tells if a list is empty.
	**/
	public function isEmpty() : Bool {
		return (h == null);
	}

	/**
		Makes the list empty.
	**/
	public function clear() : Void {
		h = null;
		length = 0;
	}

	/**
		Remove the first element that is [== v] from the list.
		Returns [true] if an element was removed, [false] otherwise.
	**/
	public function remove( v : T ) : Bool {
		var prev = null;
		#if php
		var l = null;
		untyped __php__("$l =& $this->h");
		while( l != null ) {
			if( l[0] == v ) {
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
		#else
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
		#end
		return false;
	}

	/**
		Returns an iterator on the elements of the list.
	**/
	public function iterator() : Iterator<T> {
		return cast {
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

	/**
		Returns a displayable representation of the String.
	**/
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

	/**
		Join the element of the list by using the separator [sep].
	**/
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

	/**
		Returns a list filtered with [f]. The returned list
		will contain all elements [x] for which [f(x) = true].
	**/
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

	/**
		Returns a new list where all elements have been converted
		by the function [f].
	**/
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

}
