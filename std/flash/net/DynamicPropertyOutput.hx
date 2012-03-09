package flash.net;

extern class DynamicPropertyOutput implements IDynamicPropertyOutput {
	function new() : Void;
	function writeDynamicProperty(name : String, value : Dynamic) : Void;
}
