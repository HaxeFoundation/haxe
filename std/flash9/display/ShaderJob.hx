package flash.display;

extern class ShaderJob extends flash.events.EventDispatcher {

	var height : Int;
	var progress(default,null) : Float;
	var shader : flash.display.Shader;
	var target : Dynamic;
	var width : Int;

	function new(?shader : flash.display.Shader, ?target : Dynamic, ?width : Int, ?height : Int) : Void;
	function cancel() : Void;
	function start(?waitForCompletion : Bool) : Void;
}
