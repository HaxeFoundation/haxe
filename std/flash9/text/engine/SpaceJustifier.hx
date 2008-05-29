package flash.text.engine;

extern class SpaceJustifier extends flash.text.engine.TextJustifier {
	function new(?locale : String, ?lineJustification : String, ?letterSpacing : Bool) : Void;
	var letterSpacing : Bool;
}
