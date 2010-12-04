package flash.text.ime;

@:final extern class CompositionAttributeRange {
	var converted : Bool;
	var relativeEnd : Int;
	var relativeStart : Int;
	var selected : Bool;
	function new(relativeStart : Int, relativeEnd : Int, selected : Bool, converted : Bool) : Void;
}
