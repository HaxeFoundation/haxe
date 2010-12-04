package flash.media;

@:final extern class SoundTransform {
	var leftToLeft : Float;
	var leftToRight : Float;
	var pan : Float;
	var rightToLeft : Float;
	var rightToRight : Float;
	var volume : Float;
	function new(vol : Float = 1, panning : Float = 0) : Void;
}
