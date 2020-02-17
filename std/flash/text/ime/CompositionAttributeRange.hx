package flash.text.ime;

extern final class CompositionAttributeRange {
	var converted : Bool;
	var relativeEnd : Int;
	var relativeStart : Int;
	var selected : Bool;
	function new(relativeStart : Int, relativeEnd : Int, selected : Bool, converted : Bool) : Void;
}
