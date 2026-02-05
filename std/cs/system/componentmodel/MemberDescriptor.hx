package cs.system.componentmodel;

/** Represents a class member, such as a property or event. This is an abstract base class. */
@:native("System.ComponentModel.MemberDescriptor")
extern class MemberDescriptor {
	/**
	 * Gets or sets an array of attributes.
	 * @return An array of type  that contains the attributes of this member.
	 */
	var AttributeArray(default, default):cs.NativeArray<cs.system.Attribute>;
	/**
	 * Gets the collection of attributes for this member.
	 * @return An  that provides the attributes for this member, or an empty collection
	 * if there are no attributes in the .
	 */
	var Attributes(default, never):cs.system.componentmodel.AttributeCollection;
	/**
	 * Gets the name of the category to which the member belongs, as specified in the .
	 * @return The name of the category to which the member belongs. If there is no ,
	 * the category name is set to the default category, .
	 */
	var Category(default, never):String;
	/**
	 * Gets the description of the member, as specified in the .
	 * @return The description of the member. If there is no , the property value is
	 * set to the default, which is an empty string ("").
	 */
	var Description(default, never):String;
	/**
	 * Gets whether this member should be set only at design time, as specified in the
	 * .
	 * @return if this member should be set only at design time;  if the member can be
	 * set during run time.
	 */
	var DesignTimeOnly(default, never):Bool;
	/**
	 * Gets the name that can be displayed in a window, such as a Properties window.
	 * @return The name to display for the member.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets a value indicating whether the member is browsable, as specified in the .
	 * @return if the member is browsable; otherwise, . If there is no , the property
	 * value is set to the default, which is .
	 */
	var IsBrowsable(default, never):Bool;
	/**
	 * Gets the name of the member.
	 * @return The name of the member.
	 */
	var Name(default, never):String;
	/**
	 * Gets the hash code for the name of the member, as specified in .
	 * @return The hash code for the name of the member.
	 */
	var NameHashCode(default, never):Int;
	/**
	 * Compares this instance to the given object to see if they are equivalent.
	 * @param obj The object to compare to the current instance.
	 * @return if equivalent; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
