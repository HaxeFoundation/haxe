package haxe;

typedef CallbackHandler<T> = (error:Null<Error>, result:T) -> Void;

/**
	A callback.

	All instances of `Callback` are one-time functions. That is, invoking a callback
	the second time is prohibited and will produce a null pointer exception.

	All callbacks in the standard library are functions which accept
	two arguments: an error (`haxe.Error`) and a result (`T`).

	Non-null `error` means an operation failed to finish successfully.
	In case of failure the value of the second argument has no meaning and should
	not be used.

	The underlying function type type is declared in `haxe.CallbackHandler`.
**/
abstract Callback<T>(CallbackHandler<T>) from CallbackHandler<T> {
	/**
		This method may be used instead of allocating an anonymous function to ignore
		the outcome of an operation.
	**/
	static public function ignore<T>(?e:Error, result:T):Void {}

	/**
		Create a callback, which ignores the result of an operation.

		TODO: type inference does not work for arguments of `fn` if `fromNoResult` is
		used through an implicit cast. Submit compiler issue.
	**/
	@:from static public inline function ignoreResult<T>(fn:(error:Null<Error>) -> Void):Callback<T> {
		return (e:Null<Error>, r:T) -> fn(e);
	}

	/**
		Report a failure.
	**/
	public inline function fail(error:Error):Void {
		//TODO: Does this "tidying up" make sense?
		//Callback is expected to be one-time and this cleanup is expected to help
		//to spot multiple calls
		var fn = this;
		this = null;

		fn(error, cast null);
	}

	/**
		Emit the result of a successful operation.
	**/
	public inline function success(result:T):Void {
		//TODO: Does this "tidying up" make sense?
		//Callback is expected to be one-time and this cleanup is expected to help
		//to spot multiple calls
		var fn = this;
		this = null;

		fn(null, result);
	}
}