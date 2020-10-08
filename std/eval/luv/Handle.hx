package eval.luv;

/**
	Handles.

	@see https://aantron.github.io/luv/luv/Luv/Handle
**/
@:coreType abstract Handle {
	/**
		Closes the given handle.
	**/
	extern static public function close(handle:Handle, callback:()->Void):Void;

	/**
		Returns `true` if the handle is active, `false` otherwise.
	**/
	static public function isActive(handle:Handle):Bool;

	/**
		Returns `true` if the handle is closing or closed, `false` otherwise.

		Note: This function should only be used between the initialization of
		the handle and the arrival of the close callback.
	**/
	static public function isClosing(handle:Handle):Bool;

	/**
		Reference the given handle.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-ref
	**/
	static public function ref(handle:Handle):Void;

	/**
		Un-reference the given handle.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-unref
	**/
	static public function unref(handle:Handle):Void;

	/**
		Returns `true` if the handle referenced, `false` otherwise.

		@see https://aantron.github.io/luv/luv/Luv/Handle/#val-has_ref
	**/
	static public function hasRef(handle:Handle):Bool;
}