package flash.text.engine;

@:final extern class SpaceJustifier extends TextJustifier {
	var letterSpacing : Bool;
	@:require(flash10_1) var maximumSpacing : Float;
	@:require(flash10_1) var minimumSpacing : Float;
	@:require(flash10_1) var optimumSpacing : Float;
	function new(?locale : String, ?lineJustification : LineJustification, letterSpacing : Bool = false) : Void;
}
