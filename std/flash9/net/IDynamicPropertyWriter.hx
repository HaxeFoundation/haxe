package flash.net;

extern interface IDynamicPropertyWriter {
	function new() : Void;
	function writeDynamicProperties(obj : Dynamic, output : flash.net.IDynamicPropertyOutput) : Void;
}
