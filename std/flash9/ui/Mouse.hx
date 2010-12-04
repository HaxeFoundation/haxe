package flash.ui;

extern class Mouse {
	@:require(flash10) static var cursor : MouseCursor;
	static function hide() : Void;
	static function show() : Void;
}
