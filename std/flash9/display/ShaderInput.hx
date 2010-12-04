package flash.display;

@:final extern class ShaderInput implements Dynamic {
	var channels(default,null) : Int;
	var height : Int;
	var index(default,null) : Int;
	var input : Dynamic;
	var width : Int;
	function new() : Void;
}
