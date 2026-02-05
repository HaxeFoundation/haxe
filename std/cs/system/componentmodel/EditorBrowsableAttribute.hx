package cs.system.componentmodel;

/** Specifies that a property or method is viewable in an editor. This class cannot be inherited. */
@:native("System.ComponentModel.EditorBrowsableAttribute")
extern class EditorBrowsableAttribute extends cs.system.Attribute {
	/**
	 * Gets the browsable state of the property or method.
	 * @return An  that is the browsable state of the property or method.
	 */
	var State(default, never):cs.system.componentmodel.EditorBrowsableState;
	@:overload(function():Void {})
	function new(state:cs.system.componentmodel.EditorBrowsableState):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
}
