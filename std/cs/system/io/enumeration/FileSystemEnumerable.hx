package cs.system.io.enumeration;

@:native("System.IO.Enumeration.FileSystemEnumerable")
extern class FileSystemEnumerable<TResult> {
	var ShouldIncludePredicate(default, default):cs.system.io.enumeration.FileSystemEnumerable_FindPredicate<TResult>;
	var ShouldRecursePredicate(default, default):cs.system.io.enumeration.FileSystemEnumerable_FindPredicate<TResult>;
	function new(directory:String, transform:cs.system.io.enumeration.FileSystemEnumerable_FindTransform<TResult>, ?options:cs.system.io.EnumerationOptions):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<TResult>;
}
