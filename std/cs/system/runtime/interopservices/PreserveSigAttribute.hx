package cs.system.runtime.interopservices;

/** Indicates that the HRESULT or  signature transformation that takes place during COM interop calls should be suppressed. */
@:native("System.Runtime.InteropServices.PreserveSigAttribute")
extern class PreserveSigAttribute extends cs.system.Attribute {
	function new():Void;
}
