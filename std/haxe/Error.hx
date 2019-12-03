package haxe;

import haxe.PosInfos;

/**
	Common class for errors.
**/
class Error<T> {
	/**
		Typed error data.
		E.g could be usable for discerning error types with `switch` statements if `T` is some enum.
	**/
	public final data:T;

	/**
		A human-readable representation of the error.
	**/
	public var message(get, never):String;

	/**
		Position where the error was thrown. By default, this is the place where the error is constructed.
	**/
	public final posInfos:PosInfos;

	public function new(data:T, ?posInfos:PosInfos) {
		this.data = data;
		this.posInfos = posInfos;
	}

	public function toString():String {
		return '$message at $posInfos';
	}

	function get_message():String {
		return Std.string(data);
	}
}
