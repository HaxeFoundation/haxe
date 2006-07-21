package flash;

#if flash_strict
typedef ColorTransform = {
	var ra : Float;
	var rb : Float;
	var ga : Float;
	var gb : Float;
	var ba : Float;
	var bb : Float;
	var aa : Float;
	var ab : Float;
}
#end

extern class Color
{
	function new(target : MovieClip) : Void;

	function setRGB(color:Int):Void;
	function getRGB():Int;

#if flash_strict
	function setTransform(transformObject:ColorTransform):Void;
	function getTransform():ColorTransform;
#else true
	function setTransform(transformObject:Dynamic):Void;
	function getTransform() : { ra : Float, rb : Float, ga : Float, gb : Float, ba : Float, bb : Float, aa : Float, ab : Float };
#end

	private static function __init__() : Void untyped {
		flash.Color = _global["Color"];
	}

}
