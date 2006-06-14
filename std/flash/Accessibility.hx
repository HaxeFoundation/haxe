package flash;

extern class Accessibility
{
	static function isActive() : Bool;
	static function updateProperties() : Void;
	// ? not documented ?
	static function sendEvent(mc : MovieClip, childID : Dynamic, event : Dynamic, isNonHtml : Bool) : Void;

	private static function __init__() : Void untyped {
		flash.Accessibility = _global["Accessibility"];
	}
}
