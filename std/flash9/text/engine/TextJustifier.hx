package flash.text.engine;

extern class TextJustifier {
	function new(locale : String, lineJustification : flash.text.engine.LineJustification) : Void;
	function clone() : flash.text.engine.TextJustifier;
	var lineJustification : flash.text.engine.LineJustification;
	var locale(default,null) : String;
	//private function setLocale(value : String) : Void;
	static function getJustifierForLocale(locale : String) : flash.text.engine.TextJustifier;
}
