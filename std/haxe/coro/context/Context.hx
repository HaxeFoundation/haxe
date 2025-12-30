package haxe.coro.context;

typedef ElementTree = Array<Any>;

/**
	An immutable context, which can be used like a map.
**/
abstract Context(ElementTree) {
	/**
		Creates a new immutable context containing the values in `tree`.
	**/
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	/**
		Clones `this` context as an `AdjustableContext`.
	**/
	public inline function clone() {
		return new AdjustableContext(this.copy());
	}

	/**
		Returns the value associated with `key`, or `null` if no such value exists.
	**/
	public inline function get<T>(key:Key<T>):Null<T> {
		return cast this[key.id];
	}

	/**
		Returns a string representation of `this` context.
	**/
	public inline function toString() {
		return this.toString();
	}

	/**
		Creates a new `AdjustableContext` containing the `elements` associations.
	**/
	static public inline function create(...elements:IElement<Any>) {
		return new AdjustableContext(new ElementTree()).with(...elements);
	}
}

/**
	Similar to `Context`, but mutable. This type should rarely be used explicitly and
	mostly appears as an intermediate type via the `Context` API.
**/
abstract AdjustableContext(ElementTree) {
	/**
		Creates a new context containing the values in `tree`.
	**/
	public inline function new(tree:ElementTree) {
		this = tree;
	}

	/**
		Adds or replaces `element` associated with `key`.
	**/
	public inline function add<T>(key:Key<T>, element:T) {
		this[key.id] = element;
		return abstract;
	}

	/**
		@see `Context.get`
	**/
	public inline function get<T>(key:Key<T>):Null<T> {
		return cast this[key.id];
	}

	/**
		Adds or replaces the `elements` associations in `this` context.
	**/
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
