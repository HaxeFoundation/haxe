/*
 * Copyright (C)2005-2018 Haxe Foundation
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
	A linked-list of elements. The list is composed of element container objects
	that are chained together. It is optimized so that adding or removing an
	element does not imply copying the whole list content every time.

	@see https://haxe.org/manual/std-List.html
**/
class List<T> {
	/**
		The length of `this` List.
	**/
	public var length(default,null) : Int = 0;

	var head:Null<ListNode<T>>;
	var tail:Null<ListNode<T>>;

	/**
		Creates a new empty list.
		@param capacity denotes prospective list size. Does not limit the size of the list.
						This amount of empty nodes will be created in advance.
	**/
	public function new(capacity:Int = 0) {
		if(capacity > 0) {
			head = new ListNode();
			var node = head;
			for(_ in 1...capacity) {
				node.connectTo(new ListNode());
				node = node.next;
			}
		}
	}

	/**
		Adds element `item` at the end of `this` List.

		`this.length` increases by 1.
	**/
	public function add( item : T ) {
		var node = if (tail == null) {
			if(head == null) {
				head = new ListNode();
			}
			tail = head;
		} else {
			if(tail.next == null) {
				tail.connectTo(new ListNode());
			}
			tail = tail.next;
		}
		node.item = item;
		node.isEmpty = false;
		length++;
	}

	/**
		Adds element `item` at the beginning of `this` List.

		`this.length` increases by 1.
	**/
	public function push( item : T ) {
		var node = if (tail == null) {
			if(head == null) {
				head = new ListNode();
			}
			tail = head;
		} else {
			var empty = if (tail.next == null) {
				new ListNode();
			} else {
				var empty = tail.next;
				empty.previous = null;
				if (empty.next == null) {
					tail.next = null;
				} else {
					tail.connectTo(empty.next);
				}
				empty;
			}
			empty.connectTo(head);
			head = empty;
		}
		node.item = item;
		node.isEmpty = false;
		length++;
	}

	/**
		Returns the first element of `this` List, or null if no elements exist.

		This function does not modify `this` List.
	**/
	public function first() : Null<T> {
		return tail == null ? null : head.item;
	}

	/**
		Returns the last element of `this` List, or null if no elements exist.

		This function does not modify `this` List.
	**/
	public function last() : Null<T> {
		return tail == null ? null : tail.item;
	}

	/**
		Returns the first element of `this` List, or null if no elements exist.

		The element is removed from `this` List.
	**/
	public function pop() : Null<T> {
		if (tail == null) return null;
		var result = head.item;
		clearNode(head);
		return result;
	}

	/**
		Returns the tail element of `this` List, or null if no elements exist.

		The element is removed from `this` List.
	**/
	public function popLast() : Null<T> {
		if(tail == null) return null;
		var result = tail.item;
		clearNode(tail);
		return result;
	}

	/**
		Tells if `this` List is empty.
	**/
	public inline function isEmpty() : Bool {
		return tail == null;
	}

	/**
		Every time an item is removed from the list, emptied node is moved to the pool.
		This method drops the pool.
	**/
	public inline function deflate() {
		if(tail != null) {
			tail.next = null;
		} else {
			head = null;
		}
	}

	/**
		Empties `this` List.

		This function does not traverse the elements, but simply sets the
		internal references to null and `this.length` to 0.
	**/
	public function clear() : Void {
		if (tail != null) {
			head = tail.next;
			if (head != null) {
				head.previous = null;
			}
		}
		tail = null;
		length = 0;
	}

	/**
		Removes the first occurrence of `v` in `this` List.

		If `v` is found by checking standard equality, it is removed from `this`
		List and the function returns true.

		Otherwise, false is returned.
	**/
	public function remove( v : T ) : Bool {
		var node = head;
		for(i in 0...length) {
			if(node.item == v) {
				clearNode(node);
				return true;
			}
			node = node.next;
		}
		return false;
	}

	/**
		Removes the last occurrence of `v` in `this` List.

		If `v` is found by checking standard equality, it is removed from `this`
		List and the function returns true.

		Otherwise, false is returned.

		This method performs the search from the end of the list to the beginning.
	**/
	public function removeLast( v : T ) : Bool {
		var node = tail;
		for(i in -length...0) {
			if(node.item == v) {
				clearNode(node);
				return true;
			}
			node = node.previous;
		}
		return false;
	}

	/**
		Returns an iterator on the elements of the list.
	**/
	public inline function iterator() : ListIterator<T> {
		return new ListIterator<T>(head);
	}

	/**
		Returns an iterator of the List indices and values.
	**/
	@:pure @:runtime public inline function keyValueIterator() : ListKeyValueIterator<T> {
		return new ListKeyValueIterator(head);
	}

	/**
		Returns a string representation of `this` List.

		The result is enclosed in { } with the individual elements being
		separated by a comma.
	**/
	public function toString() {
		var s = new StringBuf();
		s.add("{");
		var node = head;
		for(i in 0...length) {
			if(i > 0) s.add(", ");
			s.add(node.item);
			node = node.next;
		}
		s.add("}");
		return s.toString();
	}

	/**
		Returns a string representation of `this` List, with `sep` separating
		each element.
	**/
	public function join(sep : String) {
		var s = new StringBuf();
		var node = head;
		for(i in 0...length) {
			if(i > 0) s.add(sep);
			s.add(node.item);
			node = node.next;
		}
		return s.toString();
	}

	/**
		Returns a list filtered with `f`. The returned list will contain all
		elements for which `f(x) == true`.
	**/
	public function filter( f : T -> Bool ) {
		var result = new List();
		var node = head;
		for(i in 0...length) {
			if(f(node.item)) {
				result.add(node.item);
			}
			node = node.next;
		}
		return result;
	}

	/**
		Returns a new list where all elements have been converted by the
		function `f`.
	**/
	public function map<X>(f : T -> X) : List<X> {
		var result = new List();
		var node = head;
		for(i in 0...length) {
			result.add(f(node.item));
			node = node.next;
		}
		return result;
	}

	/**
		Reverse the order of items in this list.
	**/
	public function reverse() : Void {
		if (tail == null) return;
		var node = head.next;
		var pool = tail.next;
		for(i in 1...length) {
			var next = node.next;
			node.connectTo(node.previous);
			node = next;
		}
		var newTail = head;
		if (pool == null) {
			newTail.next = null;
		} else {
			newTail.connectTo(pool);
		}
		head = tail;
		head.previous = null;
		tail = newTail;
	}

	/**
		Create a new copy of this list.
	**/
	public function copy() : List<T> {
		var result = new List();
		var node = head;
		for(i in 0...length) {
			result.add(node.item);
			node = node.next;
		}
		return result;
	}

	inline function clearNode(node:ListNode<T>) {
		node.item = null;
		node.isEmpty = true;
		length--;

		if (length == 0) {
			tail = null;
		} else {
			if (node == tail) {
				tail = node.previous;
			} else {
				if (node == head) {
					head = node.next;
					head.previous = null;
				} else {
					node.previous.connectTo(node.next);
					if (tail.next == null) {
						node.next = null;
					} else {
						node.connectTo(tail.next);
					}
				}
				tail.connectTo(node);
			}
		}
	}
}

private class ListNode<T> {
	static var cnt = 0;
	@:keep var id = cnt++;
	public var next:ListNode<T>;
	public var previous:ListNode<T>;
	public var item:Null<T>;
	public var isEmpty:Bool = true;

	public function new() {}

	public inline function connectTo(nextNode:ListNode<T>) {
		next = nextNode;
		nextNode.previous = this;
	}
}

private class ListIterator<T> {
	var current:Null<ListNode<T>>;

	public inline function new(head:Null<ListNode<T>>) {
		this.current = head;
	}

	public inline function hasNext():Bool {
		return current != null && !current.isEmpty;
	}

	public inline function next():T {
		var val = current.item;
		current = current.next;
		return val;
	}
}

private class ListKeyValueIterator<T> {
	var idx:Int;
	var current:Null<ListNode<T>>;

	public inline function new(head:Null<ListNode<T>>) {
		this.current = head;
		this.idx = 0;
	}

	public inline function hasNext():Bool {
		return current != null && !current.isEmpty;
	}

	public inline function next():{key:Int,value:T} {
		var val = current.item;
		current = current.next;
		return {value: val, key:idx++};
	}
}
