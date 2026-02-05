package cs.system.componentmodel;

/** Specifies the type of persistence to use when serializing a property on a component at design time. */
@:native("System.ComponentModel.DesignerSerializationVisibilityAttribute")
extern class DesignerSerializationVisibilityAttribute extends cs.system.Attribute {
	/** Specifies that a serializer should serialize the contents of the property, rather than the property itself. This field is read-only. */
	static var Content(default, never):cs.system.componentmodel.DesignerSerializationVisibilityAttribute;
	/** Specifies the default value, which is , that is, a visual designer uses default rules to generate the value of a property. This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DesignerSerializationVisibilityAttribute;
	/** Specifies that a serializer should not serialize the value of the property. This  field is read-only. */
	static var Hidden(default, never):cs.system.componentmodel.DesignerSerializationVisibilityAttribute;
	/** Specifies that a serializer should be allowed to serialize the value of the property. This  field is read-only. */
	static var Visible(default, never):cs.system.componentmodel.DesignerSerializationVisibilityAttribute;
	/**
	 * Gets a value indicating the basic serialization mode a serializer should use
	 * when determining whether and how to persist the value of a property.
	 * @return One of the  values. The default is .
	 */
	var Visibility(default, never):cs.system.componentmodel.DesignerSerializationVisibility;
	function new(visibility:cs.system.componentmodel.DesignerSerializationVisibility):Void;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj Another object to compare to.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this object.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Gets a value indicating whether the current value of the attribute is the
	 * default value for the attribute.
	 * @return if the attribute is set to the default value; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
