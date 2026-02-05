package cs.system;

/** Represents a position in a non-contiguous set of memory. Properties of this type should not be interpreted by anything but the type that created it. */
@:native("System.SequencePosition")
extern class SequencePosition extends cs.system.ValueType {
	function new(object:Dynamic, integer:Int):Void;
	@:overload(function(obj:Dynamic):Bool {})
	/**
	 * Returns a value that indicates whether the current instance is equal to another
	 * object.
	 * @param obj The object to compare with the current instance.
	 * @return if  is of type  and is equal to the current instance; otherwise, .
	 */
	function Equals(other:cs.system.SequencePosition):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return The hash code for this instance.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the integer part of this .
	 * @return The integer part of this sequence position.
	 */
	function GetInteger():Int;
	/**
	 * Returns the object part of this .
	 * @return The object part of this sequence position.
	 */
	function GetObject():Dynamic;
}
