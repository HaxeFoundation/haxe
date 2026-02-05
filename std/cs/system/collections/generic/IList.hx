package cs.system.collections.generic;

@:native("System.Collections.Generic.IList")
extern interface IList<T> extends cs.system.collections.generic.ICollection<T> extends cs.system.collections.generic.IEnumerable<T> extends cs.system.collections.IEnumerable {
	@:native("get_Item")
	function get_Item(index0:Int):T;
	@:native("set_Item")
	function set_Item(index0:Int, value:T):Void;
	function IndexOf(item:T):Int;
	function Insert(index:Int, item:T):Void;
	function RemoveAt(index:Int):Void;
}
