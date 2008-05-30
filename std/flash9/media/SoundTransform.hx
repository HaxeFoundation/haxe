package flash.media;

extern class SoundTransform {
	var leftToLeft : Float;
	var leftToRight : Float;
	var pan : Float;
	var rightToLeft : Float;
	var rightToRight : Float;
	var volume : Float;
	function new(?vol : Float, ?panning : Float) : Void;
}
