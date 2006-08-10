package flash.net;

extern interface IDynamicPropertyOutput {
	function new() : Void;
	function writeDynamicProperty(name : String, value : Dynamic) : Void;
}
