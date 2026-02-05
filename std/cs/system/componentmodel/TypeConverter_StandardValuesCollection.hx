package cs.system.componentmodel;

@:native("System.ComponentModel.TypeConverter.StandardValuesCollection")
extern class TypeConverter_StandardValuesCollection {
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):Dynamic;
	function new(values:cs.system.collections.ICollection):Void;
	function CopyTo(array:cs.system.Array, index:Int):Void;
	function GetEnumerator():cs.system.collections.IEnumerator;
}
