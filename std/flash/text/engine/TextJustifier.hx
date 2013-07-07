package flash.text.engine;

extern class TextJustifier {
	var lineJustification : LineJustification;
	var locale(default,null) : String;
	function new(locale : String, lineJustification : LineJustification) : Void;
	function clone() : TextJustifier;
	static function getJustifierForLocale(locale : String) : TextJustifier;
}
