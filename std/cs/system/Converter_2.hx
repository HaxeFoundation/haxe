package cs.system;

@:native("System.Converter`2")
extern class Converter_2<TInput, TOutput> extends cs.system.MulticastDelegate {
	function new(func:(input:TInput)->TOutput):Void;
	function Invoke(input:TInput):TOutput;
}
