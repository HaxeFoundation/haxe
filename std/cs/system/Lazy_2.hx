package cs.system;

@:native("System.Lazy`2")
extern class Lazy_2<T, TMetadata> extends cs.system.Lazy_1<T0> {
	var Metadata(default, never):TMetadata;
	@:overload(function(metadata:TMetadata):Void {})
	@:overload(function(valueFactory:cs.system.Func_1<T>, metadata:TMetadata):Void {})
	@:overload(function(metadata:TMetadata, isThreadSafe:Bool):Void {})
	@:overload(function(metadata:TMetadata, mode:cs.system.threading.LazyThreadSafetyMode):Void {})
	@:overload(function(valueFactory:cs.system.Func_1<T>, metadata:TMetadata, isThreadSafe:Bool):Void {})
	function new(valueFactory:cs.system.Func_1<T>, metadata:TMetadata, mode:cs.system.threading.LazyThreadSafetyMode):Void;
}
