package flash.ui;

extern class Mouse {
	@:require(flash10) static var cursor : MouseCursor;
	@:require(flash10) static var supportsCursor(default,null) : Bool;
	static function hide() : Void;
	static function show() : Void;
}
