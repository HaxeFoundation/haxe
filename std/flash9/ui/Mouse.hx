package flash.ui;

extern class Mouse {
	static function hide() : Void;
	static function show() : Void;
	#if flash10
	static var cursor : MouseCursor;
	#end
}
