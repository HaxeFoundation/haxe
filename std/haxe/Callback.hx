package haxe;

typedef CallbackHandler<T> = (error:Null<Error>, result:T) -> Void;

/**
	A callback.

	All callbacks in the standard library are functions which accept
	two arguments: an error (`haxe.Error`) and a result (`T`). Non-null `error` means
	an operation failed to finish successfully.

	The callback type is declared in `haxe.CallbackHandler`.

	TODO:
	This abstract is introduced for potential callback API improvements.
**/
@:callable
abstract Callback<T>(CallbackHandler<T>) from CallbackHandler<T> {
	/**
		Create a callback for an operation, which does not produce any result data.

		TODO: type inference does not work for arguments of `fn` if `fromNoResult` is
		used through an implicit cast. Submit compiler issue.
	**/
	@:from static public inline function fromNoResult(fn:(error:Null<Error>) -> Void):Callback<NoData> {
		return (e:Null<Error>, _) -> fn(e);
	}
}