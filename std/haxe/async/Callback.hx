package haxe.async;

import haxe.Error;
import haxe.NoData;

typedef CallbackData<T> = (?error:Error, ?result:T) -> Void;

/**
	A callback. All callbacks in the standard library are functions which accept
	two arguments: an error (`haxe.Error`) and a result (`T`). If error is 
	non-`null`, result must be `null`. The callback type is declared in 	`CallbackData`.

	This abstract defines multiple `@:from` conversions to improve readability of
	callback code.
**/
@:callable
abstract Callback<T>(CallbackData<T>) from CallbackData<T> {
	/**
		Returns a callback of the same type as `cb` which is guaranteed to be
		non-`null`. If `cb` is given and is not `null` it is returned directly.
		If `cb` is `null` a dummy callback which does nothing is returned instead.
	**/
	public static function nonNull<T>(?cb:Callback<T>):Callback<T> {
		if (cb == null)
			return (_, _) -> {};
		return cb;
	}

	/**
		Wraps a function which takes a single optional `haxe.Error` argument into
		a callback of type `Callback<NoData>`. Allows:

		```haxe
		var cb:Callback<NoData> = (?err) -> trace("error!", err);
		```
	**/
	@:from public static inline function fromOptionalErrorOnly(f:(?error:Error) -> Void):Callback<NoData> {
		return (?err:Error, ?result:NoData) -> f(err);
	}

	/**
		Wraps a function which takes a single `haxe.Error` argument into a callback
		of type `Callback<NoData>`. Allows:

		```haxe
		var cb:Callback<NoData> = (err) -> trace("error!", err);
		```
	**/
	@:from public static inline function fromErrorOnly(f:(error:Error) -> Void):Callback<NoData> {
		return (?err:Error, ?result:NoData) -> f(err);
	}

	/*
	// this should not be encouraged, may mess up from(Optional)ErrorOnly
	@:from static inline function fromResultOnly<T>(f:(?result:T) -> Void):Callback<T> return (?err:Error, ?result:T) -> f(result);
	*/

	/**
		Wraps a callback function declared without `?` (optional) arguments into a
		callback.
	**/
	@:from public static inline function fromErrorResult<T>(f:(error:Error, result:T) -> Void):Callback<T> {
		return (?err:Error, ?result:T) -> f(err, result);
	}

	#if (hl || neko)
	private inline function toUVNoData() return (error) -> this(error, null);
	#end
}
