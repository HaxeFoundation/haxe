package haxe.coro.context;

import haxe.ds.BalancedTree;

class ElementTree extends BalancedTree<Key<Any>, IElement<Any>> {
	override function compare(k1:Key<Any>, k2:Key<Any>) {
		return k2.id - k1.id;
	}

	override function copy():ElementTree {
		var copied = new ElementTree();
		copied.root = root;
		return copied;
	}

	override function toString() {
		var buf = new StringBuf();
		var first = true;
		for (key => value in this) {
			if (!first) {
				buf.add(", ");
			} else {
				first = false;
			}
			buf.add('${key.name}: $value');
		}
		return buf.toString();
	}
}

abstract Context(ElementTree) {
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	public function clone() {
		return new AdjustableContext(this.copy());
	}

	public function get<T>(key:Key<T>):T {
		return cast this.get(key);
	}

	public function toString() {
		return this.toString();
	}

	static public function create(...elements:IElement<Any>) {
		return new AdjustableContext(new ElementTree()).with(...elements);
	}
}

abstract AdjustableContext(ElementTree) {
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	public function with(...elements:IElement<Any>) {
		for (element in elements) {
			this.set(element.getKey(), element);
		}
		return abstract;
	}

	@:to function toContext():Context {
		return new Context(this);
	}
}