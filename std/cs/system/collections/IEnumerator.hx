package cs.system.collections;

@:native('System.Collections.IEnumerator') extern interface IEnumerator
{
	var Current(default, null):Dynamic;
	function MoveNext():Bool;
	function Reset():Void;
}