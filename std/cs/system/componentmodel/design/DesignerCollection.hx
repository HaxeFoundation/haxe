package cs.system.componentmodel.design;

/** Represents a collection of designers. */
@:native("System.ComponentModel.Design.DesignerCollection")
extern class DesignerCollection {
	/**
	 * Gets the number of designers in the collection.
	 * @return The number of designers in the collection.
	 */
	var Count(default, never):Int;
	@:native("get_Item")
	function get_Item(index0:Int):cs.system.componentmodel.design.IDesignerHost;
	@:overload(function(designers:cs.system.collections.IList):Void {})
	function new(designers:cs.NativeArray<cs.system.componentmodel.design.IDesignerHost>):Void;
	/**
	 * Gets a new enumerator for this collection.
	 * @return An  that enumerates the collection.
	 */
	function GetEnumerator():cs.system.collections.IEnumerator;
}
