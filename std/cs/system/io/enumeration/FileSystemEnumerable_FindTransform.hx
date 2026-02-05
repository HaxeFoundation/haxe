package cs.system.io.enumeration;

@:native("System.IO.Enumeration.FileSystemEnumerable`1.FindTransform")
extern class FileSystemEnumerable_FindTransform<TResult> extends cs.system.MulticastDelegate {
	function new(func:(entry:cs.system.io.enumeration.FileSystemEntry)->TResult):Void;
	function Invoke(entry:cs.Ref<cs.system.io.enumeration.FileSystemEntry>):TResult;
}
