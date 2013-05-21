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
	that are chained together. It is optimized so that adding or removing an
	element does not imply copying the whole array content every time.
**/
class List<T> {

	private var h : Array<Dynamic>;
	private var q : Array<Dynamic>;

	/**
		The length of [this] List.
	**/
	public var length(default,null) : Int;

	/**
		Creates a new empty list.
	**/
	public function new() {
		length = 0;
	}

	/**
		Adds element [item] at the end of [this] List.
		
		[this].length increases by 1.
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
		Adds element [item] at the beginning of [this] List.
		
		[this].length increases by 1.
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
		Returns the first element of [this] List, or null if no elements exist.
		
		This function does not modify [this] List.
	**/
	public function first() : Null<T> {
		return if( h == null ) null else h[0];
	}

	/**
		Returns the last element of [this] List, or null if no elements exist.
		
		This function does not modify [this] List.
	**/
	public function last() : Null<T> {
		return if( q == null ) null else q[0];
	}


	/**
		Returns the first element of [this] List, or null if no elements exist.
		
		The element is removed from [this] List.
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
		Tells if [this] List is empty.
	**/
	public function isEmpty() : Bool {
		return (h == null);
	}

	/**
		Empties [this] List.
		
		This function does not traverse the elements, but simply sets the
		internal references to null and [this].length to 0.
	**/
	public function clear() : Void {
		h = null;
		q = null;
		length = 0;
	}

	/**
		Removes the first occurence of [v] in [this] List.
		
		If [v] is found by checking standard equality, it is removed from [this]
		List and the function returns true.
		
		Otherwise, false is returned.
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
		Returns a string representation of [this] List.
		
		The result is enclosed in { } with the individual elements being
		separated by a comma.
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
		Returns a string representation of [this] List, with [sep] separating
		each element.
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
		Returns a list filtered with [f]. The returned list will contain all
		elements for which [f(x) = true].
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
		Returns a new list where all elements have been converted by the
		function [f].
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
