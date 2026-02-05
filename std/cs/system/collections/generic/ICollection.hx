package cs.system.collections.generic;

@:native("System.Collections.Generic.ICollection")
extern interface ICollection<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable {
	var Count(default, never):Int;
	var IsReadOnly(default, never):Bool;
	function Add(item:T):Void;
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, arrayIndex:Int):Void;
	function Remove(item:T):Bool;
}
