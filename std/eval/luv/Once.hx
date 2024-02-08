package eval.luv;

/**
	Once-only initialization.

	@see https://aantron.github.io/luv/luv/Luv/Once
**/
@:coreType abstract Once {
	/**
		Allocates and initializes a once-only barrier.
	**/
	static public function init():Result<Once>;

	/**
		Guards the given callback to be called only once.
	**/
	public function once(callback:()->Void):Void;
}