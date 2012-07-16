package cs.system.collections;

@:native('System.Collections.ICollection') extern interface ICollection implements IEnumerable
{
	var Count(default, null):Int;
	var IsSynchronized(default, null):Bool;
	var SyncRoot(default, null):Bool;
	
	function CopyTo(arr:cs.system.Array, index:Int):Void;
	function GetEnumerator():IEnumerator;
}