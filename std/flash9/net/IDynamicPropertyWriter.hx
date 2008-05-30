package flash.net;

extern interface IDynamicPropertyWriter {
	function writeDynamicProperties(obj : Dynamic, output : IDynamicPropertyOutput) : Void;
}
