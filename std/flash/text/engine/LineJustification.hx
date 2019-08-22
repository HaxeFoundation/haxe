package flash.text.engine;

@:native("flash.text.engine.LineJustification") extern enum abstract LineJustification(String) {
	var ALL_BUT_LAST;
	var ALL_BUT_MANDATORY_BREAK;
	var ALL_INCLUDING_LAST;
	var UNJUSTIFIED;
}
