package cs.system.componentmodel;

/** Provides a read-only container for a collection of  objects. */
@:native("System.ComponentModel.ComponentCollection")
extern class ComponentCollection extends cs.system.collections.ReadOnlyCollectionBase {
	@:overload(function(index0:Int):cs.system.componentmodel.IComponent {})
	@:native("get_Item")
	function get_Item(index0:String):cs.system.componentmodel.IComponent;
	function new(components:cs.NativeArray<cs.system.componentmodel.IComponent>):Void;
	/**
	 * Copies the entire collection to an array, starting writing at the specified
	 * array index.
	 * @param array An  array to copy the objects in the collection to.
	 * @param index The index of the  at which copying to should begin.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.componentmodel.IComponent>, index:Int):Void;
}
