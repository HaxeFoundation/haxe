package haxe.coro.context;

class KeyChainNode {
	public final key:Int;
	public final value:Any;
	public var next:Null<KeyChainNode>;

	public function new(key:Int, value:Any) {
		this.key = key;
		this.value = value;
	}

	@:keep public function toString() {
		return '$key => $value';
	}
}

class KeyChain {
	static final unused = new KeyChainNode(-1, null);

	var root:Null<KeyChainNode>;
	var last:KeyChainNode;

	public function new() {
		last = unused;
	}

	public function get(key:Int) {
		if (last.key == key) {
			return last.value;
		}
		var current = root;
		while (true) {
			if (current == null) {
				return null;
			}
			if (current.key == key) {
				last = current;
				return current.value;
			}
			current = current.next;
		}
	}

	public function set(key:Int, value:Any) {
		last = new KeyChainNode(key, value);
		if (root == null) {
			root = last;
		} else if (root.key == key) {
			last.next = root.next;
			root = last;
		} else {
			var current = new KeyChainNode(root.key, root.value);
			last.next = current;
			current.next = root.next;
			root = last;
			while (true) {
				if (current.next == null) {
					// new element
					break;
				} else if (current.next.key == key) {
					// keep tail
					current.next = current.next.next;
					break;
				} else {
					final newCurrent = new KeyChainNode(current.next.key, current.next.value);
					newCurrent.next = current.next.next;
					current = newCurrent;
				}
			}
		}
	}

	public function copy() {
		final newChain = new KeyChain();
		newChain.root = root;
		return newChain;
	}

	public function toString() {
		var buf = new StringBuf();
		buf.add("[");
		var current = root;
		while (current != null) {
			buf.add(current.key);
			buf.add(" => ");
			buf.add(current.value);
			if (current.next != null) {
				buf.add(", ");
				current = current.next;
			} else {
				break;
			}
		}
		buf.add("]");
		return buf.toString();
	}
}

typedef ElementTree = KeyChain;

abstract Context(ElementTree) {
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	public function clone() {
		return new AdjustableContext(this.copy());
	}

	public function get<T>(key:Key<T>):T {
		return cast this.get(key.id);
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

	public function add<T>(key:Key<T>, element:T) {
		this.set(key.id, element);
		return abstract;
	}

	public function with(...elements:IElement<Any>) {
		for (element in elements) {
			this.set(element.getKey().id, element);
		}
		return abstract;
	}

	@:to function toContext():Context {
		return new Context(this);
	}
}
