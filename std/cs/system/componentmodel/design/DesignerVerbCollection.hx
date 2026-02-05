package cs.system.componentmodel.design;

/** Represents a collection of  objects. */
@:native("System.ComponentModel.Design.DesignerVerbCollection")
extern class DesignerVerbCollection extends cs.system.collections.CollectionBase {
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.componentmodel.design.DesignerVerb;
	@:native("set_Item")
	function set_Item(index0:Int, value:cs.system.componentmodel.design.DesignerVerb):Void;
	@:overload(function():Void {})
	function new(value:cs.NativeArray<cs.system.componentmodel.design.DesignerVerb>):Void;
	/**
	 * Adds the specified  to the collection.
	 * @param value The  to add to the collection.
	 * @return The index in the collection at which the verb was added.
	 */
	function Add(value:cs.system.componentmodel.design.DesignerVerb):Int;
	@:overload(function(value:cs.system.componentmodel.design.DesignerVerbCollection):Void {})
	/**
	 * Adds the specified set of designer verbs to the collection.
	 * @param value An array of  objects to add to the collection.
	 */
	function AddRange(value:cs.NativeArray<cs.system.componentmodel.design.DesignerVerb>):Void;
	/**
	 * Gets a value indicating whether the specified  exists in the collection.
	 * @param value The  to search for in the collection.
	 * @return if the specified object exists in the collection; otherwise, .
	 */
	function Contains(value:cs.system.componentmodel.design.DesignerVerb):Bool;
	/**
	 * Copies the collection members to the specified  array beginning at the specified
	 * destination index.
	 * @param array The array to copy collection members to.
	 * @param index The destination index to begin copying to.
	 */
	function CopyTo(array:cs.NativeArray<cs.system.componentmodel.design.DesignerVerb>, index:Int):Void;
	/**
	 * Gets the index of the specified .
	 * @param value The  whose index to get in the collection.
	 * @return The index of the specified object if it is found in the list; otherwise,
	 * -1.
	 */
	function IndexOf(value:cs.system.componentmodel.design.DesignerVerb):Int;
	/**
	 * Inserts the specified  at the specified index.
	 * @param index The index in the collection at which to insert the verb.
	 * @param value The  to insert in the collection.
	 */
	function Insert(index:Int, value:cs.system.componentmodel.design.DesignerVerb):Void;
	/**
	 * Removes the specified  from the collection.
	 * @param value The  to remove from the collection.
	 */
	function Remove(value:cs.system.componentmodel.design.DesignerVerb):Void;
}
