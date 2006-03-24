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
class Lambda<A,B> {

	/**
		Creates an [Array] from an [Iterator]
	**/
	public static function array( it : Iterator<A> ) : Array<A> {
		var a = new Array<A>();
		for(i in it)
			a.push(i);
		return a;
	}

	/**
		Creates a new [Iterator] that will apply the function 'f' to all
		elements of the iterator 'it'.
	**/
	public static function map( it : Iterator<A>, f : A -> B ) : Iterator<B> {
		// untyped is required because anonymous objects are not interfaces
		return untyped {
			hasNext : it.hasNext,
			next : function() { return f(it.next()); }
		}
	}

	/**
		Call the function 'f' on all elements of the [Iterator] 'it'.
	**/
	public static function iter( it : Iterator<A>, f : A -> Void ) {
		for( x in it )
			f(x);
	}

	/**
		Creates an [Array] 'b' from an [Array] 'a' by applying the function 'f'
		on all elements of 'a'.
	**/
	public static function amap(a : Array<A>,f : A -> B) : Array<B> {
		var b = new Array();
		for( x in a )
			b.push(f(x));
		return b;
	}

	/**
		Functional 'fold' using an [Iterator]
	**/
	public static function fold( it : Iterator<A>, f : A -> B -> B, first : B ) : B {
		for( x in it )
			first = f(x,first);
		return first;
	}

	/**
		Count the number of elements in an [Iterator]
	**/
	public static function count( it : Iterator<A> ) {
		var n = 0;
		for( _ in it )
			++n;
		return n;
	}

}