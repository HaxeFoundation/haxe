package cs.system.data;

/** Represents a collection of properties that can be added to , , or . */
@:native("System.Data.PropertyCollection")
extern class PropertyCollection extends cs.system.collections.Hashtable {
	function new():Void;
	/**
	 * Creates a shallow copy of the  object.
	 * @return Returns , a shallow copy of the  object.
	 */
	function Clone():Dynamic;
}
