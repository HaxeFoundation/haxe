package flash.text.engine;

extern class SpaceJustifier extends flash.text.engine.TextJustifier {
	function new(?locale : String, ?lineJustification : flash.text.engine.LineJustification, ?letterSpacing : Bool) : Void;
	var letterSpacing : Bool;
}
