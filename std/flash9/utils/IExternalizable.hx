package flash.utils;

extern interface IExternalizable {
	function readExternal(input : flash.utils.IDataInput) : Void;
	function writeExternal(output : flash.utils.IDataOutput) : Void;
}
