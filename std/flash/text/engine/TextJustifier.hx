package flash.text.engine;

extern class TextJustifier {
	@:flash.property var lineJustification(get,set) : LineJustification;
	@:flash.property var locale(get,never) : String;
	function new(locale : String, lineJustification : LineJustification) : Void;
	function clone() : TextJustifier;
	private function get_lineJustification() : LineJustification;
	private function get_locale() : String;
	private function set_lineJustification(value : LineJustification) : LineJustification;
	static function getJustifierForLocale(locale : String) : TextJustifier;
}
