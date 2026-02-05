package cs.system;

@:native("System.Converter")
extern class Converter<TInput, TOutput> extends cs.system.MulticastDelegate {
	function new(func:(input:TInput)->TOutput):Void;
	function Invoke(input:TInput):TOutput;
}
