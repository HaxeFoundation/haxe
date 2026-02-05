package cs.system.collections.objectmodel;

@:native("System.Collections.ObjectModel.Collection")
extern class Collection<T> {
	var Count(default, never):Int;
	var Items(default, never):cs.system.collections.generic.IList<T>;
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:native("set_Item")
	function set_Item(index0:Int, value:T):Void;
	@:overload(function():Void {})
	function new(list:cs.system.collections.generic.IList<T>):Void;
	function Add(item:T):Void;
	function Clear():Void;
	function Contains(item:T):Bool;
	function CopyTo(array:cs.NativeArray<T>, index:Int):Void;
	function GetEnumerator():cs.system.collections.generic.IEnumerator<T>;
	function IndexOf(item:T):Int;
	function Insert(index:Int, item:T):Void;
	function Remove(item:T):Bool;
	function RemoveAt(index:Int):Void;
}
