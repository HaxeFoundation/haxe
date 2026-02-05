package cs.system;

@:native("System.Lazy")
extern class Lazy<T, TMetadata> extends cs.system.Lazy {
	var Metadata(default, never):TMetadata;
	@:overload(function(metadata:TMetadata):Void {})
	@:overload(function(valueFactory:cs.system.Func, metadata:TMetadata):Void {})
	@:overload(function(metadata:TMetadata, isThreadSafe:Bool):Void {})
	@:overload(function(metadata:TMetadata, mode:cs.system.threading.LazyThreadSafetyMode):Void {})
	@:overload(function(valueFactory:cs.system.Func, metadata:TMetadata, isThreadSafe:Bool):Void {})
	function new(valueFactory:cs.system.Func, metadata:TMetadata, mode:cs.system.threading.LazyThreadSafetyMode):Void;
}
