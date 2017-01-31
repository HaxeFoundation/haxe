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
	BalancedTree allows key-value mapping with arbitrary keys, as long as they
	can be ordered. By default, `Reflect.compare` is used in the `compare`
	method, which can be overridden in subclasses.

	Operations have a logarithmic average and worst-case cost.

	Iteration over keys and values, using `keys` and `iterator` respectively,
	are in-order.
**/
class BalancedTree<K,V> {
	var root:TreeNode<K,V>;

	/**
		Creates a new BalancedTree, which is initially empty.
	**/
	public function new() { }

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
			if (c == 0) return node.value;
			if (c < 0) node = node.left;
			else node = node.right;
		}
		return null;
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
		}
		catch (e:String) {
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
			if (c == 0) return true;
			else if (c < 0) node = node.left;
			else node = node.right;
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
		Iterates over the keys of `this` BalancedTree.

		This operation is performed in-order.
	**/
	public function keys():Iterator<K> {
		var ret = [];
		keysLoop(root, ret);
		return ret.iterator();
	}

	function setLoop(k:K, v:V, node:TreeNode<K,V>) {
		if (node == null) return new TreeNode<K,V>(null, k, v, null);
		var c = compare(k, node.key);
		return if (c == 0) new TreeNode<K,V>(node.left, k, v, node.right, node.get_height());
		else if (c < 0) {
			var nl = setLoop(k, v, node.left);
			balance(nl, node.key, node.value, node.right);
		} else {
			var nr = setLoop(k, v, node.right);
			balance(node.left, node.key, node.value, nr);
		}
	}

	function removeLoop(k:K, node:TreeNode<K,V>) {
		if (node == null) throw "Not_found";
		var c = compare(k, node.key);
		return if (c == 0) merge(node.left, node.right);
		else if (c < 0) balance(removeLoop(k, node.left), node.key, node.value, node.right);
		else balance(node.left, node.key, node.value, removeLoop(k, node.right));
	}

	function iteratorLoop(node:TreeNode<K,V>, acc:Array<V>) {
		if (node != null) {
			iteratorLoop(node.left, acc);
			acc.push(node.value);
			iteratorLoop(node.right, acc);
		}
	}

	function keysLoop(node:TreeNode<K,V>, acc:Array<K>) {
		if (node != null) {
			keysLoop(node.left, acc);
			acc.push(node.key);
			keysLoop(node.right, acc);
		}
	}

	function merge(t1, t2) {
		if (t1 == null) return t2;
		if (t2 == null) return t1;
		var t = minBinding(t2);
		return balance(t1, t.key, t.value, removeMinBinding(t2));
	}

	function minBinding(t:TreeNode<K,V>) {
		return if (t == null) throw "Not_found";
		else if (t.left == null) t;
		else minBinding(t.left);
	}

	function removeMinBinding(t:TreeNode<K,V>) {
		return if (t.left == null) t.right;
		else balance(removeMinBinding(t.left), t.key, t.value, t.right);
	}

	function balance(l:TreeNode<K,V>, k:K, v:V, r:TreeNode<K,V>):TreeNode<K,V> {
		var hl = l.get_height();
		var hr = r.get_height();
		return if (hl > hr + 2) {
			if (l.left.get_height() >= l.right.get_height()) new TreeNode<K,V>(l.left, l.key, l.value, new TreeNode<K,V>(l.right, k, v, r));
			else new TreeNode<K,V>(new TreeNode<K,V>(l.left,l.key, l.value, l.right.left), l.right.key, l.right.value, new TreeNode<K,V>(l.right.right, k, v, r));
		} else if (hr > hl + 2) {
			if (r.right.get_height() > r.left.get_height()) new TreeNode<K,V>(new TreeNode<K,V>(l, k, v, r.left), r.key, r.value, r.right);
			else new TreeNode<K,V>(new TreeNode<K,V>(l, k, v, r.left.left), r.left.key, r.left.value, new TreeNode<K,V>(r.left.right, r.key, r.value, r.right));
		} else {
			new TreeNode<K,V>(l, k, v, r, (hl > hr ? hl : hr) + 1);
		}
	}

	function compare(k1:K, k2:K) {
		return Reflect.compare(k1, k2);
	}

	public function toString() {
		return root == null ? '{}' : '{${root.toString()}}';
	}
}

/**
	A tree node of `haxe.ds.BalancedTree`.
**/
class TreeNode<K,V> {
	public var left : TreeNode<K,V>;
	public var right : TreeNode<K,V>;
	public var key : K;
	public var value : V;
	#if as3
	public
	#end
	var _height : Int;

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

	@:extern public inline function get_height() return this == null ? 0 : _height;

	public function toString() {
		return (left == null ? "" : left.toString() + ", ") + '$key=$value' + (right == null ? "" : ", " +right.toString());
	}
}