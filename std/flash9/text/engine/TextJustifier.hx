package flash.text.engine;

extern class TextJustifier {
	function new(locale : String, lineJustification : String) : Void;
	var lineJustification : String;
	var locale(default,null) : String;
	//private function setLocale(value : String) : Void;
	static function getJustifierForLocale(locale : String) : flash.text.engine.TextJustifier;
}
