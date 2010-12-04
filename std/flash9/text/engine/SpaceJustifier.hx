package flash.text.engine;

@:final extern class SpaceJustifier extends TextJustifier {
	var letterSpacing : Bool;
	function new(?locale : String, ?lineJustification : LineJustification, letterSpacing : Bool = false) : Void;
}
