package haxe;

import haxe.PosInfos;

/**
	Common class for errors.
**/
class Error {
	/**
		Error message.
	**/
	public var message(default, null):String;

	/**
		Position where the error was thrown. By default, this is the place where the error is constructed.
	**/
	public final posInfos:PosInfos;

	public function new(message:String, ?posInfos:PosInfos) {
		this.message = message;
		this.posInfos = posInfos;
	}

	/**
		Error description.
	**/
	public function toString():String {
		return '$message at ${pos()}';
	}

	function pos():String {
		return posInfos.fileName + ':' + posInfos.lineNumber;
	}
}
