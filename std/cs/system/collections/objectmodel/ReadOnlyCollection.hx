package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.ReadOnlyCollection")
extern class ReadOnlyCollection<T> {
	var Count(default, never):Int;
	var Items(default, never):cs.system.collections.generic.IList<T>;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	function new(list:cs.system.collections.generic.IList<T>):Void;
	function Contains(value:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function IndexOf(value:T):Int;
}
