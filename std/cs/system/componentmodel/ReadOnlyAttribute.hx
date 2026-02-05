package cs.system.componentmodel;

/** Specifies whether the property this attribute is bound to is read-only or read/write. This class cannot be inherited */
@:native("System.ComponentModel.ReadOnlyAttribute")
extern class ReadOnlyAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is  (that is, the property this attribute is bound to is read/write). This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.ReadOnlyAttribute;
	/** Specifies that the property this attribute is bound to is read/write and can be modified. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.ReadOnlyAttribute;
	/** Specifies that the property this attribute is bound to is read-only and cannot be modified in the server explorer. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.ReadOnlyAttribute;
	/**
	 * Gets a value indicating whether the property this attribute is bound to is
	 * read-only.
	 * @return if the property this attribute is bound to is read-only;  if the
	 * property is read/write.
	 */
	var IsReadOnly(default, never):Bool;
	function new(isReadOnly:Bool):Void;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param value Another object to compare to.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
