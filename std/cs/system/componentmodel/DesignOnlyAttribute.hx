package cs.system.componentmodel;

/** Specifies whether a property can only be set at design time. */
@:native("System.ComponentModel.DesignOnlyAttribute")
extern class DesignOnlyAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.DesignOnlyAttribute;
	/** Specifies that a property can be set at design time or at run time. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.DesignOnlyAttribute;
	/** Specifies that a property can be set only at design time. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.DesignOnlyAttribute;
	/**
	 * Gets a value indicating whether a property can be set only at design time.
	 * @return if a property can be set only at design time; otherwise, .
	 */
	var IsDesignOnly(default, never):Bool;
	function new(isDesignOnly:Bool):Void;
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
	/**
	 * Determines if this attribute is the default.
	 * @return if the attribute is the default value for this attribute class;
	 * otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
