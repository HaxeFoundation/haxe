package validation;

import haxe.macro.Expr;

class Exception {
	public var msg(default,null):String;
	public var pos(default,null):Null<Position>;

	public function new(msg:String, ?pos:Position) {
		this.msg = msg;
		this.pos = pos;
	}
}

class ExpectedOutputException extends Exception {}

class InvalidTargetException extends Exception {}