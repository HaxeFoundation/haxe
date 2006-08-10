package flash.media;

extern class SoundTransform {
	function new(?vol : Float, ?panning : Float) : Void;
	var leftToLeft : Float;
	var leftToRight : Float;
	var pan : Float;
	var rightToLeft : Float;
	var rightToRight : Float;
	var volume : Float;
}
