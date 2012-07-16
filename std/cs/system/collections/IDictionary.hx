package cs.system.collections;

@:native('System.Collections.IDictionary') extern interface IDictionary implements ICollection, implements ArrayAccess<Dynamic>
{
	var IsFixedSize(default, null):Bool;
	var IsReadOnly(default, null):Bool;
	
	function Add(key:Dynamic, value:Dynamic):Void;
	function Clear():Void;
	function Contains(key:Dynamic):Bool;
	function Remove(key:Dynamic):Void;
	function GetEnumerator():IDictionaryEnumerator;
}

@:native('System.Collections.IDictionaryEnumerator') extern interface IDictionaryEnumerator implements IEnumerator
{
	var Key(default, null):Dynamic;
	var Value(default, null):Dynamic;
}