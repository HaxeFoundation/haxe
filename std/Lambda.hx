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
		Count the number of elements in an [Iterable]
	**/
	public static function count<A>( it : Iterable<A> ) {
		var n = 0;
		for( _ in it )
			++n;
		return n;
	}

	/**
		Tells if an iterable does not contain any element.
	**/
	public static function empty( it : Iterable<Dynamic> ) : Bool {
		return !it.iterator().hasNext();
	}

}
