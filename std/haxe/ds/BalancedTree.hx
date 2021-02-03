/*
 * Copyright (C)2005-2019 Haxe Foundation
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
	BalancedTree allows key-value mapping with arbitrary keys, as long as they
	can be ordered. By default, `Reflect.compare` is used in the `compare`
	method, which can be overridden in subclasses.

	Operations have a logarithmic average and worst-case cost.

	Iteration over keys and values, using `keys` and `iterator` respectively,
	are in-order.
**/
class BalancedTree<K, V> implements haxe.Constraints.IMap<K, V> {
	var root:TreeNode<K, V>;


	/**
		Creates a new BalancedTree, which is initially empty.
	**/
	public function new() {}

	/**
		Binds `key` to `value`.

		If `key` is already bound to a value, that binding disappears.

		If `key` is null, the result is unspecified.
	**/
	public function set(key:K, value:V) {
		root = setLoop(key, value, root);
	}

	/**
		Returns the value `key` is bound to.

		If `key` is not bound to any value, `null` is returned.

		If `key` is null, the result is unspecified.
	**/
	public function get(key:K):Null<V> {
		var node = root;
		while (node != null) {
			var c = compare(key, node.key);
			if (c == 0)
				return node.value;
			if (c < 0)
				node = node.left;
			else
				node = node.right;
		}
		return null;
	}

	/**
		Returns the key and bound value of the largest key in this map less
		than or equal to `key`.
		
		If `key` is present in this map, returns it and its bound value.
		If `key` is not present, the key immediately _before_ this key is found,
		and it and the corresponding bound value are returned.

		If there is no such key in this map, both the returned key and value
		will be `null`.

		If `key` is omitted or `null`, returns the minimum key and its bound value
		(equivalent to `min()`).

		If the map is empty, both the returned key and value will be `null`.
		
		The returned struct is guaranteed non-null (only the fields may be null).
	**/
	public function floor(?key:K): Entry<K,V> {
		var node = root;
		var floor = new TreeNode<K, V>(null, null, null, null, 0);

		if (key == null) {
			return min();
		}

		while (node != null) {
			var c = compare(key, node.key);
			if (c == 0)
				return {key:node.key, value:node.value};
			if (c < 0) {
				node = node.left;
			} else {
				floor = node; // biggest node we've found that's smaller than the key
				node = node.right;
			}
		}
		return {key:floor.key, value:floor.value};
	}

	/**
		Returns the key and bound value of the smallest key in this map greater
		than or equal to `key`.
		
		If `key` is present in this map, returns it and its bound value.
		If `key` is not present, the key immediately _after_ this key is found,
		and it and the corresponding bound value are returned.

		If there is no such key in this map, both the returned key and value
		will be `null`.

		If `key` is omitted or `null`, returns the maximum key and its bound value
		(eqivalent to `max()`).

		If the map is empty, both the returned key and value will be `null`.
		
		The returned struct is guaranteed non-null (only the fields may be null).
	**/
	public function ceil(?key:K): Entry<K,V> {
		var node = root;
		var ceil = new TreeNode<K, V>(null, null, null, null, 0);

		if (key == null) {
			return max();
		}

		while (node != null) {
			var c = compare(key, node.key);
			if (c == 0)
				return {key:node.key, value:node.value};
			if (c < 0) {
				ceil = node; // biggest node we've found that's smaller than the key
				node = node.left;
			} else {
				node = node.right;
			}
		}
		return {key:ceil.key, value:ceil.value};
	}

	/**
		Returns the first / minimal key and the corresponding bound value.

		If the map is empty, both the returned key and value will be `null`.
		
		The returned struct is guaranteed non-null (only the fields may be null).
	**/
	public function min(): Entry<K,V> {
		var node = minChild(root);
		if (node == null) {
			return {key:null, value:null};
		} else {
			return {key:node.key, value:node.value};
		}
	}

	/**
		Returns the last / maximal key and the corresponding bound value.
		
		If the map is empty, both the returned key and value will be `null`.
		
		The returned struct is guaranteed non-null (only the fields may be null).
	**/
	public function max(): Entry<K,V> {
		var node = maxChild(root);
		if (node == null) {
			return {key:null, value:null};
		} else {
			return {key:node.key, value:node.value};
		}
	}

	/**
		Returns the key and bound value of the specified key (if present)
		and the keys and values that are immediately before and after it in
		order (if there is another key in that direction).
		
		If `key` is present in this map, `ident` in the return struct is
		that key and its value, otherwise both `ident.key` and `ident.value`
		are `null`.

		If there is a key in this map before `key`, `prev` in the return
		struct is the key and value of the key immediately before `key`,
		otherwise both `prev.key` and `prev.value` are `null`.
		If `key` is omitted or `null`, `prev` is instead the minimum key
		and bound value (equivalent to the value returned by `min()`).

		If there is a key this map after `key`, `next` in the return
		struct is the key and value of the key immediately after `key`,
		otherwise both `next.key` and `next.value` are `null`.
		If `key` is omitted or `null`, `next` is instead the minimum key
		and bound value (equivalent to the value returned by `max()`).

		Note that if the map is empty, all returned keys and values will
		be `null`.
		
		The returned struct and the sub-structs `prev`, `ident`, and `next`
		are guaranteed non-null (only the `key` and `value` fields may be null).
	**/
	public function neighborhood(?key:K): Neighborhood<K,V> {
		var node = root;
		var prev, ident, next;
		var empty = new TreeNode<K, V>(null, null, null, null, 0);
		prev = ident = next = empty;

		if (key == null) {
			if (node != null) {
				prev = minChild(node);
				next = maxChild(node);
			}
		} else {
			while (node != null) {
				var c = compare(key, node.key);
				if (c == 0) {
					ident = node;
					if (node.left != null) prev = maxChild(node.left);
					if (node.right != null) next = minChild(node.right);
					break;
				}
				if (c < 0) {
					next = node; // smallest node we've found that's bigger than the key
					node = node.left;
				} else {
					prev = node; // biggest node we've found that's smaller than the key
					node = node.right;
				}
			}
		}
		return {prev:{key:prev.key, value:prev.value}, ident:{key:ident.key, value:ident.value}, next:{key:next.key, value:next.value}};
	}

	/**
		Seek along the right branch to the last non-null child.
		Returns a `null` only if passed a `null`.
	**/
	function maxChild(node:TreeNode<K, V>): TreeNode<K, V> {
		if (node != null)
			while (node.right != null)
				node = node.right;
		return node;
	}
	/**
		Seek along the left branch to the last non-null child.
		Returns a `null` only if passed a `null`.
	**/
	function minChild(node:TreeNode<K, V>): TreeNode<K, V> {
		if (node != null)
			while (node.left != null)
				node = node.left;
		return node;
	}

	/**
		Removes the current binding of `key`.

		If `key` has no binding, `this` BalancedTree is unchanged and false is
		returned.

		Otherwise the binding of `key` is removed and true is returned.

		If `key` is null, the result is unspecified.
	**/
	public function remove(key:K) {
		try {
			root = removeLoop(key, root);
			return true;
		} catch (e:String) {
			return false;
		}
	}

	/**
		Tells if `key` is bound to a value.

		This method returns true even if `key` is bound to null.

		If `key` is null, the result is unspecified.
	**/
	public function exists(key:K) {
		var node = root;
		while (node != null) {
			var c = compare(key, node.key);
			if (c == 0)
				return true;
			else if (c < 0)
				node = node.left;
			else
				node = node.right;
		}
		return false;
	}

	/**
		Iterates over the bound values of `this` BalancedTree.

		This operation is performed in-order.
	**/
	public function iterator():Iterator<V> {
		var ret = [];
		iteratorLoop(root, ret);
		return ret.iterator();
	}

	/**
		See `Map.keyValueIterator`
	**/
	@:runtime public inline function keyValueIterator():KeyValueIterator<K, V> {
		return new haxe.iterators.MapKeyValueIterator(this);
	}

	/**
		Iterates over the keys of `this` BalancedTree.

		This operation is performed in-order.
	**/
	public function keys():Iterator<K> {
		var ret = [];
		keysLoop(root, ret);
		return ret.iterator();
	}

	public function copy():BalancedTree<K, V> {
		var copied = new BalancedTree<K, V>();
		copied.root = root;
		return copied;
	}

	function setLoop(k:K, v:V, node:TreeNode<K, V>) {
		if (node == null)
			return new TreeNode<K, V>(null, k, v, null);
		var c = compare(k, node.key);
		return if (c == 0) new TreeNode<K, V>(node.left, k, v, node.right, node.get_height()); else if (c < 0) {
			var nl = setLoop(k, v, node.left);
			balance(nl, node.key, node.value, node.right);
		} else {
			var nr = setLoop(k, v, node.right);
			balance(node.left, node.key, node.value, nr);
		}
	}

	function removeLoop(k:K, node:TreeNode<K, V>) {
		if (node == null)
			throw "Not_found";
		var c = compare(k, node.key);
		return if (c == 0) merge(node.left,
			node.right); else if (c < 0) balance(removeLoop(k, node.left), node.key, node.value,
			node.right); else balance(node.left, node.key, node.value, removeLoop(k, node.right));
	}

	static function iteratorLoop<K,V>(node:TreeNode<K, V>, acc:Array<V>) {
		if (node != null) {
			iteratorLoop(node.left, acc);
			acc.push(node.value);
			iteratorLoop(node.right, acc);
		}
	}

	function keysLoop(node:TreeNode<K, V>, acc:Array<K>) {
		if (node != null) {
			keysLoop(node.left, acc);
			acc.push(node.key);
			keysLoop(node.right, acc);
		}
	}

	function merge(t1, t2) {
		if (t1 == null)
			return t2;
		if (t2 == null)
			return t1;
		var t = minBinding(t2);
		return balance(t1, t.key, t.value, removeMinBinding(t2));
	}

	function minBinding(t:TreeNode<K, V>) {
		return if (t == null) throw "Not_found"; else if (t.left == null) t; else minBinding(t.left);
	}

	function removeMinBinding(t:TreeNode<K, V>) {
		return if (t.left == null) t.right; else balance(removeMinBinding(t.left), t.key, t.value, t.right);
	}

	function balance(l:TreeNode<K, V>, k:K, v:V, r:TreeNode<K, V>):TreeNode<K, V> {
		var hl = l.get_height();
		var hr = r.get_height();
		return if (hl > hr + 2) {
			if (l.left.get_height() >= l.right.get_height())
				new TreeNode<K, V>(l.left, l.key, l.value, new TreeNode<K, V>(l.right, k, v, r));
			else
				new TreeNode<K, V>(new TreeNode<K, V>(l.left, l.key, l.value, l.right.left), l.right.key, l.right.value,
					new TreeNode<K, V>(l.right.right, k, v, r));
		} else if (hr > hl + 2) {
			if (r.right.get_height() > r.left.get_height())
				new TreeNode<K, V>(new TreeNode<K, V>(l, k, v, r.left), r.key, r.value, r.right);
			else
				new TreeNode<K, V>(new TreeNode<K, V>(l, k, v, r.left.left), r.left.key, r.left.value,
					new TreeNode<K, V>(r.left.right, r.key, r.value, r.right));
		} else {
			new TreeNode<K, V>(l, k, v, r, (hl > hr ? hl : hr) + 1);
		}
	}

	function compare(k1:K, k2:K) {
		return Reflect.compare(k1, k2);
	}

	public function toString() {
		return root == null ? '{}' : '{${root.toString()}}';
	}

	/**
		Removes all keys from `this` BalancedTree.
	**/
	public function clear():Void {
		root = null;
	}
}

/**
	A key and its associated value.
**/
typedef Entry<K,V> = {key:Null<K>, value:Null<V>};
/**
	The entry `ident` corresponding to the queried key, plus the entries immediately before and after it in the mapping.
**/
typedef Neighborhood<K,V> = {prev:Entry<K,V>, ident:Entry<K,V>, next:Entry<K,V>};

/**
	A tree node of `haxe.ds.BalancedTree`.
**/
class TreeNode<K, V> {
	public var left:TreeNode<K, V>;
	public var right:TreeNode<K, V>;
	public var key:K;
	public var value:V;

	var _height:Int;

	public function new(l, k, v, r, h = -1) {
		left = l;
		key = k;
		value = v;
		right = r;
		if (h == -1)
			_height = (left.get_height() > right.get_height() ? left.get_height() : right.get_height()) + 1;
		else
			_height = h;
	}

	extern public inline function get_height()
		return this == null ? 0 : _height;

	public function toString() {
		return (left == null ? "" : left.toString() + ", ") + '$key=$value' + (right == null ? "" : ", " + right.toString());
	}
}
