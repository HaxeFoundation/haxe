package haxe.coro.context;

typedef ElementTree = Array<Any>;

abstract Context(ElementTree) {
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	public inline function clone() {
		return new AdjustableContext(this.copy());
	}

	public inline function get<T>(key:Key<T>):T {
		return cast this[key.id];
	}

	public inline function toString() {
		return this.toString();
	}

	static public inline function create(...elements:IElement<Any>) {
		return new AdjustableContext(new ElementTree()).with(...elements);
	}
}

abstract AdjustableContext(ElementTree) {
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	public inline function add<T>(key:Key<T>, element:T) {
		this[key.id] = element;
		return abstract;
	}

	public inline function get<T>(key:Key<T>):T {
		return cast this[key.id];
	}

	public function with(...elements:IElement<Any>) {
		for (element in elements) {
			this[element.getKey().id] = element;
		}
		return abstract;
	}

	@:to inline function toContext():Context {
		return new Context(this);
	}
}
