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

package haxe.ds;

/**
	ListSort provides a stable implementation of merge sort through its `sort`
	method. It has a O(N.log(N)) complexity and does not require additional memory allocation.
**/
class ListSort {

	// Note : we prefer [inline] over [@:generic] here since we want to inline the comparison function as well

	/**
		Sorts List `lst` according to the comparison function `cmp`, where
		`cmp(x,y)` returns 0 if `x == y`, a positive Int if `x > y` and a
		negative Int if `x < y`.

		This operation modifies List `a` in place and returns its head once modified.
		The `prev` of the head is set to the tail of the sorted list.

		If `list` or `cmp` are null, the result is unspecified.
	**/
	public static inline function sort<T:{prev:T,next:T}>(list:T, cmp : T -> T -> Int) : T {
		// ported from http://www.chiark.greenend.org.uk/~sgtatham/algorithms/listsort.html
		if( list == null )
			return null;
		var insize = 1, nmerges, psize = 0, qsize = 0;
		var p, q, e, tail : T = null;
		while( true ) {
			p = list;
			list = null;
			tail = null;
			nmerges = 0;
			while( p != null ) {
				nmerges++;
				q = p;
				psize = 0;
				for( i in 0...insize ) {
					psize++;
					q = q.next;
					if( q == null ) break;
				}
				qsize = insize;
				while( psize > 0 || (qsize > 0 && q != null) ) {
					if( psize == 0 ) {
						e = q;
						q = q.next;
						qsize--;
					} else if( qsize == 0 || q == null || cmp(p,q) <= 0 ) {
						e = p;
						p = p.next;
						psize--;
					} else {
						e = q;
						q = q.next;
						qsize--;
					}
					if( tail != null )
						tail.next = e;
					else
						list = e;
					e.prev = tail;
					tail = e;
				}
				p = q;
			}
			tail.next = null;
			if( nmerges <= 1 )
				break;
			insize *= 2;
		}
		list.prev = tail;
		return list;
	}


	/**
		Same as `sort` but on single linked list.
	**/
	public static inline function sortSingleLinked<T:{next:T}>(list:T, cmp : T -> T -> Int) : T {
		if( list == null )
			return null;
		var insize = 1, nmerges, psize = 0, qsize = 0;
		var p, q, e, tail : T;
		while( true ) {
			p = list;
			list = null;
			tail = null;
			nmerges = 0;
			while( p != null ) {
				nmerges++;
				q = p;
				psize = 0;
				for( i in 0...insize ) {
					psize++;
					q = q.next;
					if( q == null ) break;
				}
				qsize = insize;
				while( psize > 0 || (qsize > 0 && q != null) ) {
					if( psize == 0 ) {
						e = q;
						q = q.next;
						qsize--;
					} else if( qsize == 0 || q == null || cmp(p,q) <= 0 ) {
						e = p;
						p = p.next;
						psize--;
					} else {
						e = q;
						q = q.next;
						qsize--;
					}
					if( tail != null )
						tail.next = e;
					else
						list = e;
					tail = e;
				}
				p = q;
			}
			tail.next = null;
			if( nmerges <= 1 )
				break;
			insize *= 2;
		}
		return list;
	}

}