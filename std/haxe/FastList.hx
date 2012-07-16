/*
 * Copyright (c) 2005-2008, The haXe Project Contributors
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
package haxe;

class FastCell<T> #if (flash9 || cpp) implements haxe.rtti.Generic #end {
	public var elt : T;
	public var next : FastCell<T>;
	public function new(elt,next) { this.elt = elt; this.next = next; }
}

#if cpp
#if haxe3
@:generic
#end
private class FastListIterator<T> extends cpp.FastIterator<T>#if !haxe3 , implements haxe.rtti.Generic #end {
	public var current : FastCell<T>;
	override public function hasNext():Bool { return current!=null; }
	override public function next():T { var result = current.elt; current = current.next; return result; }

	public function new(head) { current = head; }
}

#end

/**
	A linked-list of elements. A different class is created for each container used in platforms where it matters
**/
#if (haxe3 && (flash9 || cpp))
@:generic
#end
class FastList<T> #if (!haxe3 && (flash9 || cpp)) implements haxe.rtti.Generic #end {

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
		return new FastListIterator<T>(head);
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
