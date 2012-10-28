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
	The [Lambda] class is a collection of functional methods in order to
	use functional-style programming with haXe.
**/
class Lambda {

	/**
		Creates an [Array] from an [Iterable]
	**/
	public static function array<A>( it : Iterable<A> ) : Array<A> {
		var a = new Array<A>();
		for(i in it)
			a.push(i);
		return a;
	}

	/**
		Creates a [List] from an [Iterable]
	**/
	public static function list<A>( it : Iterable<A> ) : List<A> {
		var l = new List<A>();
		for(i in it)
			l.add(i);
		return l;
	}

	/**
		Creates a new [Iterable] by appling the function 'f' to all
		elements of the iterator 'it'.
	**/
	public static function map<A,B>( it : Iterable<A>, f : A -> B ) : List<B> {
		var l = new List<B>();
		for( x in it )
			l.add(f(x));
		return l;
	}

	/**
		Similar to [map], but also pass an index for each item iterated.
	**/
	public static function mapi<A,B>( it : Iterable<A>, f : Int -> A -> B ) : List<B> {
		var l = new List<B>();
		var i = 0;
		for( x in it )
			l.add(f(i++,x));
		return l;
	}

	/**
		Tells if the element is part of an iterable. The comparison
		is made using the [==] operator. Optionally you can pass as
		a third parameter a function that performs the comparison.
		That function must take as arguments the two items to
		compare and returns a boolean value.
	**/
	public static function has<A>( it : Iterable<A>, elt : A, ?cmp : A -> A -> Bool ) : Bool {
		if( cmp == null ) {
			for( x in it )
				if( x == elt )
					return true;
		} else {
			for( x in it )
				if( cmp(x,elt) )
					return true;
		}
		return false;
	}

	/**
		Tells if at least one element of the iterable is found by using the specific function.
	**/
	public static function exists<A>( it : Iterable<A>, f : A -> Bool ) {
		for( x in it )
			if( f(x) )
				return true;
		return false;
	}

	/**
		Tells if all elements of the iterable have the specified property defined by [f].
	**/
	public static function foreach<A>( it : Iterable<A>, f : A -> Bool ) {
		for( x in it )
			if( !f(x) )
				return false;
		return true;
	}

	/**
		Call the function 'f' on all elements of the [Iterable] 'it'.
	**/
	public static function iter<A>( it : Iterable<A>, f : A -> Void ) {
		for( x in it )
			f(x);
	}

	/**
		Return the list of elements matching the function 'f'
	**/
	public static function filter<A>( it : Iterable<A>, f : A -> Bool ) {
		var l = new List<A>();
		for( x in it )
			if( f(x) )
				l.add(x);
		return l;
	}

	/**
		Functional 'fold' using an [Iterable]
	**/
	public static function fold<A,B>( it : Iterable<A>, f : A -> B -> B, first : B ) : B {
		for( x in it )
			first = f(x,first);
		return first;
	}

	/**
		Count the number of elements in an [Iterable] having [pred] returning true.
	**/
	public static function count<A>( it : Iterable<A>, ?pred : A -> Bool ) {
		var n = 0;
		if( pred == null )
			for( _ in it )
				n++;
		else
			for( x in it )
				if( pred(x) )
					n++;
		return n;
	}

	/**
		Tells if an iterable does not contain any element.
	**/
	public static function empty( it : Iterable<Dynamic> ) : Bool {
		return !it.iterator().hasNext();
	}

	/**
		Returns the index of the item in the given Iterable, depending on the order of the Iterator.
		Returns -1 if the item was not found.
	**/
	public static function indexOf<T>( it : Iterable<T>, v : T ) : Int {
		var i = 0;
		for( v2 in it ) {
			if( v == v2 )
				return i;
			i++;
		}
		return -1;
	}

	/**
		Returns a list containing all items of 'a' followed by all items of 'b'
	**/
	public static function concat<T>( a : Iterable<T>, b : Iterable<T> ) : List<T> {
		var l = new List();
		for( x in a )
			l.add(x);
		for( x in b )
			l.add(x);
		return l;
	}

}
