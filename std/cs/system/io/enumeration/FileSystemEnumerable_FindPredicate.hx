package cs.system.io.enumeration;

@:native("System.IO.Enumeration.FileSystemEnumerable`1.FindPredicate")
extern class FileSystemEnumerable_FindPredicate<TResult> extends cs.system.MulticastDelegate {
	function new(func:(entry:cs.system.io.enumeration.FileSystemEntry)->Bool):Void;
	function Invoke(entry:cs.Ref<cs.system.io.enumeration.FileSystemEntry>):Bool;
}
