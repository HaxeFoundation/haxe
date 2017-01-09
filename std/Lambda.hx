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

/**
	The `Lambda` class is a collection of methods to support functional
	programming. It is ideally used with `using Lambda` and then acts as an
	extension to Iterable types.

	On static platforms, working with the Iterable structure might be slower
	than performing the operations directly on known types, such as Array and
	List.

	If the first argument to any of the methods is null, the result is
	unspecified.

	@see https://haxe.org/manual/std-Lambda.html
**/
class Lambda {

	/**
		Creates an Array from Iterable `it`.

		If `it` is an Array, this function returns a copy of it.
	**/
	public static function array<A>( it : Iterable<A> ) : Array<A> {
		var a = new Array<A>();
		for(i in it)
			a.push(i);
		return a;
	}

	/**
		Creates a List form Iterable `it`.

		If `it` is a List, this function returns a copy of it.
	**/
	public static function list<A>( it : Iterable<A> ) : List<A> {
		var l = new List<A>();
		for(i in it)
			l.add(i);
		return l;
	}

	/**
		Creates a new List by applying function `f` to all elements of `it`.

		The order of elements is preserved.

		If `f` is null, the result is unspecified.
	**/
	public static function map<A,B>( it : Iterable<A>, f : A -> B ) : List<B> {
		var l = new List<B>();
		for( x in it )
			l.add(f(x));
		return l;
	}

	/**
		Similar to map, but also passes the index of each element to `f`.

		The order of elements is preserved.

		If `f` is null, the result is unspecified.
	**/
	public static function mapi<A,B>( it : Iterable<A>, f : Int -> A -> B ) : List<B> {
		var l = new List<B>();
		var i = 0;
		for( x in it )
			l.add(f(i++,x));
		return l;
	}

	/**
		Concatenate a list of lists.

		The order of elements is preserved.
	**/
	public static function flatten<A>( it : Iterable<Iterable<A>> ) : List<A> {
		var l = new List<A>();
		for (e in it)
			for (x in e)
				l.add(x);
		return l;
	}

	/**
		A composition of map and flatten.

		The order of elements is preserved.

		If `f` is null, the result is unspecified.
	**/
	public static function flatMap<A,B>( it : Iterable<A>, f: A -> Iterable<B> ) : List<B> {
		return Lambda.flatten(Lambda.map(it, f));
	}

	/**
		Tells if `it` contains `elt`.

		This function returns true as soon as an element is found which is equal
		to `elt` according to the `==` operator.

		If no such element is found, the result is false.
	**/
	public static function has<A>( it : Iterable<A>, elt : A ) : Bool {
		for( x in it )
			if( x == elt )
				return true;
		return false;
	}

	/**
		Tells if `it` contains an element for which `f` is true.

		This function returns true as soon as an element is found for which a
		call to `f` returns true.

		If no such element is found, the result is false.

		If `f` is null, the result is unspecified.
	**/
	public static function exists<A>( it : Iterable<A>, f : A -> Bool ) {
		for( x in it )
			if( f(x) )
				return true;
		return false;
	}

	/**
		Tells if `f` is true for all elements of `it`.

		This function returns false as soon as an element is found for which a
		call to `f` returns false.

		If no such element is found, the result is true.

		In particular, this function always returns true if `it` is empty.

		If `f` is null, the result is unspecified.
	**/
	public static function foreach<A>( it : Iterable<A>, f : A -> Bool ) {
		for( x in it )
			if( !f(x) )
				return false;
		return true;
	}

	/**
		Calls `f` on all elements of `it`, in order.

		If `f` is null, the result is unspecified.
	**/
	public static function iter<A>( it : Iterable<A>, f : A -> Void ) {
		for( x in it )
			f(x);
	}

	/**
		Returns a List containing those elements of `it` for which `f` returned
		true.

		If `it` is empty, the result is the empty List even if `f` is null.

		Otherwise if `f` is null, the result is unspecified.
	**/
	public static function filter<A>( it : Iterable<A>, f : A -> Bool ) {
		var l = new List<A>();
		for( x in it )
			if( f(x) )
				l.add(x);
		return l;
	}

	/**
		Functional fold on Iterable `it`, using function `f` with start argument
		`first`.

		If `it` has no elements, the result is `first`.

		Otherwise the first element of `it` is passed to `f` alongside `first`.
		The result of that call is then passed to `f` with the next element of
		`it`, and so on until `it` has no more elements.

		If `it` or `f` are null, the result is unspecified.
	**/
	public static function fold<A,B>( it : Iterable<A>, f : A -> B -> B, first : B ) : B {
		for( x in it )
			first = f(x,first);
		return first;
	}

	/**
		Returns the number of elements in `it` for which `pred` is true, or the
		total number of elements in `it` if `pred` is null.

		This function traverses all elements.
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
		Tells if Iterable `it` does not contain any element.
	**/
	public static function empty<T>( it : Iterable<T> ) : Bool {
		return !it.iterator().hasNext();
	}

	/**
		Returns the index of the first element `v` within Iterable `it`.

		This function uses operator `==` to check for equality.

		If `v` does not exist in `it`, the result is -1.
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
		Returns the first element of `it` for which `f` is true.

		This function returns as soon as an element is found for which a call to
		`f` returns true.

		If no such element is found, the result is null.

		If `f` is null, the result is unspecified.
	**/
	public static function find<T>( it : Iterable<T>, f : T -> Bool ) : Null<T> {
		for( v in it ) {
			if(f(v)) return v;
		}
		return null;
	}

	/**
		Returns a new List containing all elements of Iterable `a` followed by
		all elements of Iterable `b`.

		If `a` or `b` are null, the result is unspecified.
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
