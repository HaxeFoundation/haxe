package flash.display;

extern class ShaderJob extends flash.events.EventDispatcher {
	var height : Int;
	var progress(default,never) : Float;
	var shader : Shader;
	var target : Dynamic;
	var width : Int;
	function new(?shader : Shader, ?target : Dynamic, width : Int = 0, height : Int = 0) : Void;
	function cancel() : Void;
	function start(waitForCompletion : Bool = false) : Void;
}
