package haxe;

typedef CallbackHandler<T> = (error:Null<Exception>, result:T) -> Void;

/**
	A callback.

	All instances of `Callback` are one-time functions. That is, invoking a callback
	the second time must never happen.

	All callbacks in the standard library are functions which accept
	two arguments: an error (`haxe.Exception`) and a result (`T`).

	Non-null `error` means an operation failed to finish successfully.
	In case of failure the value of the second argument has no meaning and should
	not be used.

	The underlying function type is declared in `haxe.CallbackHandler`.
**/
abstract Callback<T>(CallbackHandler<T>) from CallbackHandler<T> {
	/**
		This method may be used instead of allocating an anonymous function to ignore
		the outcome of an operation.
	**/
	static public function ignore<T>(?e:Exception, result:T):Void {}

	/**
		Create a callback, which ignores the result of an operation.

		TODO: type inference does not work for arguments of `fn` if `fromNoResult` is
		used through an implicit cast. Submit compiler issue.
	**/
	@:from static public inline function ignoreResult<T>(fn:(error:Null<Exception>) -> Void):Callback<T> {
		return (e:Null<Exception>, r:T) -> fn(e);
	}

	/**
		Report a failure.
	**/
	public inline function fail(error:Exception):Void {
		this(error, cast null);
	}

	/**
		Emit the result of a successful operation.
	**/
	public inline function success(result:T):Void {
		this(null, result);
	}
}