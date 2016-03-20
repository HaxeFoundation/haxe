package flash.display;

@:final extern class ShaderInput implements Dynamic {
	var channels(default,never) : Int;
	var height : Int;
	var index(default,never) : Int;
	var input : Dynamic;
	var width : Int;
	function new() : Void;
}
