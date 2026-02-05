package cs.system.collections.specialized;

@:native("System.Collections.Specialized.NameObjectCollectionBase.KeysCollection")
extern class NameObjectCollectionBase_KeysCollection {
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):String;
	function Get(index:Int):String;
	function GetEnumerator():cs.system.collections.IEnumerator;
}
