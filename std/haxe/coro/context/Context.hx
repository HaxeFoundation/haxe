package haxe.coro.context;

import haxe.exceptions.CoroutineException;

typedef ElementTree = Array<Null<Any>>;

/**
	An immutable context, which can be used like a map.
**/
abstract Context(ElementTree) {

	/**
		The empty context.
	**/
	static public final empty = Context.create();

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

	public inline function getOrRaise<T>(key:Key<T>):T {
		return get(key) ?? throw new CoroutineException("Key " + key.name + " not found in context");
	}

	/**
		Returns a string representation of `this` context.
	**/
	public inline function toString() {
		return this.toString();
	}

	/**
		Returns a copy of `this` context with `elements` added to it, potentially
		overwriting existing associations.
	**/
	public function with(...elements:IElement<Any>):Context {
		return clone().with(...elements);
	}

	/**
		Returns a copy of `this` context where all elements associated with `keys`
		are unset.
	**/
	public function without(...keys:Key<Any>):Context {
		return clone().without(...keys);
	}

	/**
		Returns a new instance of `Context` where all elements from `context` are
		added to the elements from `this` context, overwriting them if necessary.
	**/
	@:op(A + B) function plus(context:Context) {
		final out = asArray().copy();
		for (k => v in context.asArray()) {
			out[k] = v;
		}
		return new Context(out);
	}

	function asArray() {
		return this;
	}

	/**
		Creates a new `Context` containing the `elements` associations.
	**/
	static public inline function create(...elements:IElement<Any>) {
		return new Context(new ElementTree()).with(...elements);
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
		@see `Context.get`
	**/
	public inline function get<T>(key:Key<T>):Null<T> {
		return cast this[key.id];
	}

	/**
		Adds or replaces `element` associated with `key`.
	**/
	public inline function set<T>(key:Key<T>, element:T) {
		this[key.id] = element;
		return abstract;
	}

	@:deprecated("Use `set` instead")
	public inline function add<T>(key:Key<T>, element:T) {
		return set(key, element);
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

	/**
		Unsets all `keys` in `this` context.
	**/
	public function without(...keys:Key<Any>) {
		for (key in keys) {
			this[key.id] = null;
		}
		return abstract;
	}

	@:to inline function toContext():Context {
		return new Context(this);
	}
}
