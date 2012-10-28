/*
 * Copyright (C)2005-2012 Haxe Foundation
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
/**
	A linked-list of elements. The list is composed of two-elements arrays
	that are chained together. It's optimized so that adding or removing an
	element doesn't imply to copy the whole array content everytime.
**/
class List<T> {

	private var h : Array<Dynamic>;
	private var q : Array<Dynamic>;

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
		var x:Array<Dynamic> = #if neko untyped __dollar__array(item,null) #else [item] #end;
		if( h == null )
			h = x;
		else
			q[1] = x;
		q = x;
		length++;
	}

	/**
		Push an element at the beginning of the list.
	**/
	public function push( item : T ) {
		var x : Array<Dynamic> = #if neko
			untyped __dollar__array(item,h)
		#else
			[item,h]
		#end;
		h = x;
		if( q == null )
			q = x;
		length++;
	}

	/**
		Returns the first element of the list, or null
		if the list is empty.
	**/
	public function first() : Null<T> {
		return if( h == null ) null else h[0];
	}

	/**
		Returns the last element of the list, or null
		if the list is empty.
	**/
	public function last() : Null<T> {
		return if( q == null ) null else q[0];
	}


	/**
		Removes the first element of the list and
		returns it or simply returns null if the
		list is empty.
	**/
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
		q = null;
		length = 0;
	}

	/**
		Remove the first element that is [== v] from the list.
		Returns [true] if an element was removed, [false] otherwise.
	**/
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

	/**
		Returns an iterator on the elements of the list.
	**/
	public function iterator() : Iterator<T> {
		#if (java || cs)
		var h = h;
		return cast {
			hasNext : function() {
				return (h != null);
			},
			next : function() {
				{
					if( h == null )
						return null;
					var x = h[0];
					h = h[1];
					return x;
				}
			}
		}
		#else
		return cast {
			h : h,
			hasNext : function() {
				return untyped (__this__.h != null);
			},
			next : function() {
				untyped {
					if( __this__.h == null )
						return null;
					var x = __this__.h[0];
					__this__.h = __this__.h[1];
					return x;
				}
			}
		}
		#end
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
			s.add(Std.string(l[0]));
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
