package cs.system.io.enumeration;

@:native("System.IO.Enumeration.FileSystemEnumerator")
extern class FileSystemEnumerator<TResult> extends cs.system.runtime.constrainedexecution.CriticalFinalizerObject {
	var Current(default, never):TResult;
	function new(directory:String, ?options:cs.system.io.EnumerationOptions):Void;
	function Dispose():Void;
	function MoveNext():Bool;
	function Reset():Void;
}
