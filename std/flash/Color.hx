package flash;

extern class Color
{

	function new(target : MovieClip) : Void;

	function setRGB(offset:Float):Void;
	function setTransform(transformObject:Dynamic):Void;
	function getRGB():Float;
	function getTransform():Dynamic;
}
