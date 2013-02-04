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
package haxe.ds;

#if (flash9 || cpp)
@:generic
#end
class FastCell<T> {
	public var elt : T;
	public var next : FastCell<T>;
	public function new(elt,next) { this.elt = elt; this.next = next; }
}

#if cpp
@:generic
private class GenericStackIterator<T> extends cpp.FastIterator<T> {
	public var current : FastCell<T>;
	override public function hasNext():Bool { return current!=null; }
	override public function next():T { var result = current.elt; current = current.next; return result; }

	public function new(head) { current = head; }
}

#end

/**
	A linked-list of elements. A different class is created for each container used in platforms where it matters
**/
#if (flash9 || cpp)
@:generic
#end
class GenericStack<T> {

	public var head : FastCell<T>;

	/**
		Creates a new empty list.
	**/
	public function new() {
	}

	/**
		Add an element at the head of the list.
	**/
	public inline function add( item : T ) {
		head = new FastCell<T>(item,head);
	}

	/**
		Returns the first element of the list, or null
		if the list is empty.
	**/
	public inline function first() : Null<T> {
		return if( head == null ) null else head.elt;
	}

	/**
		Removes the first element of the list and
		returns it or simply returns null if the
		list is empty.
	**/
	public inline function pop() : Null<T> {
		var k = head;
		if( k== null )
			return null;
		else {
			head = k.next;
			return k.elt;
		}
	}

	/**
		Tells if a list is empty.
	**/
	public inline function isEmpty() : Bool {
		return (head == null);
	}

	/**
		Remove the first element that is [== v] from the list.
		Returns [true] if an element was removed, [false] otherwise.
	**/
	public function remove( v : T ) : Bool {
		var prev = null;
		var l = head;
		while( l != null ) {
			if( l.elt == v ) {
				if( prev == null )
					head = l.next;
				else
					prev.next = l.next;
				break;
			}
			prev = l;
			l = l.next;
		}
		return (l != null);
	}

	#if cpp

	/**
		Returns an iterator on the elements of the list.
	**/
	public function iterator() : Iterator<T> {
		return new GenericStackIterator<T>(head);
	}

	#else

	/**
		Returns an iterator on the elements of the list.
	**/
	public function iterator() : Iterator<T> {
		var l = head;
		return {
			hasNext : function() {
				return l != null;
			},
			next : function() {
				var k = l;
				l = k.next;
				return k.elt;
			}
		};
	}
   #end

	/**
		Returns a displayable representation of the String.
	**/
	public function toString() {
		var a = new Array();
		var l = head;
		while( l != null ) {
			a.push(l.elt);
			l = l.next;
		}
		return "{"+a.join(",")+"}";
	}

}
