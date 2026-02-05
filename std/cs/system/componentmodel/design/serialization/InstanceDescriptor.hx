package cs.system.componentmodel.design.serialization;

/** Provides the information necessary to create an instance of an object. This class cannot be inherited. */
@:native("System.ComponentModel.Design.Serialization.InstanceDescriptor")
extern class InstanceDescriptor {
	/**
	 * Gets the collection of arguments that can be used to reconstruct an instance of
	 * the object that this instance descriptor represents.
	 * @return An  of arguments that can be used to create the object.
	 */
	var Arguments(default, never):cs.system.collections.ICollection;
	/**
	 * Gets a value indicating whether the contents of this  completely identify the
	 * instance.
	 * @return if the instance is completely described; otherwise, .
	 */
	var IsComplete(default, never):Bool;
	/**
	 * Gets the member information that describes the instance this descriptor is
	 * associated with.
	 * @return A  that describes the instance that this object is associated with.
	 */
	var MemberInfo(default, never):cs.system.reflection.MemberInfo;
	@:overload(function(member:cs.system.reflection.MemberInfo, arguments:cs.system.collections.ICollection):Void {})
	function new(member:cs.system.reflection.MemberInfo, arguments:cs.system.collections.ICollection, isComplete:Bool):Void;
	/**
	 * Invokes this instance descriptor and returns the object the descriptor
	 * describes.
	 * @return The object this instance descriptor describes.
	 */
	function Invoke():Dynamic;
}
