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
	A cell of `haxe.ds.GenericStack`.
  
	@see https://haxe.org/manual/std-GenericStack.html
**/
#if (flash || cpp)
@:generic
#end
class GenericCell<T> {
	public var elt : T;
	public var next : GenericCell<T>;
	public function new(elt,next) { this.elt = elt; this.next = next; }
}

#if cpp
@:generic
private class GenericStackIterator<T> extends cpp.FastIterator<T> {
	public var current : GenericCell<T>;
	override public function hasNext():Bool { return current!=null; }
	override public function next():T { var result = current.elt; current = current.next; return result; }

	public function new(head) { current = head; }
}

#end

/**
	A stack of elements.

	This class is generic, which means one type is generated for each type
	parameter T on static targets. For example:

	- `new GenericStack<Int>()` generates `GenericStack_Int`
	- `new GenericStack<String>()` generates `GenericStack_String`

	The generated name is an implementation detail and should not be relied
	upon.

	@see https://haxe.org/manual/std-GenericStack.html
**/
#if (flash || cpp)
@:generic
#end
class GenericStack<T> {

	public var head : GenericCell<T>;

	/**
		Creates a new empty GenericStack.
	**/
	public function new() {
	}

	/**
		Pushes element `item` onto the stack.
	**/
	public inline function add( item : T ) {
		head = new GenericCell<T>(item,head);
	}

	/**
		Returns the topmost stack element without removing it.

		If the stack is empty, null is returned.
	**/
	public inline function first() : Null<T> {
		return if( head == null ) null else head.elt;
	}

	/**
		Returns the topmost stack element and removes it.

		If the stack is empty, null is returned.
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
		Tells if the stack is empty.
	**/
	public inline function isEmpty() : Bool {
		return (head == null);
	}

	/**
		Removes the first element which is equal to `v` according to the `==`
		operator.

		This method traverses the stack until it finds a matching element and
		unlinks it, returning true.

		If no matching element is found, false is returned.
	**/
	public function remove( v : T ) : Bool {
		var prev:GenericCell<T> = null;
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
		Returns an iterator over the elements of `this` GenericStack.
	**/
	public function iterator() : Iterator<T> {
		return new GenericStackIterator<T>(head);
	}

	#else

	/**
		Returns an iterator over the elements of `this` GenericStack.
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
		Returns a String representation of `this` GenericStack.
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
