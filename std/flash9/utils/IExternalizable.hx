package flash.utils;

extern interface IExternalizable {
	function new() : Void;
	function readExternal(input : flash.utils.IDataInput) : Void;
	function writeExternal(output : flash.utils.IDataOutput) : Void;
}
