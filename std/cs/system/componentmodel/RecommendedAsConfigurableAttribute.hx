package cs.system.componentmodel;

/** Specifies that the property can be used as an application setting. */
@:native("System.ComponentModel.RecommendedAsConfigurableAttribute")
extern class RecommendedAsConfigurableAttribute extends cs.system.Attribute {
	/** Specifies the default value for the , which is . This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.RecommendedAsConfigurableAttribute;
	/** Specifies that a property cannot be used as an application setting. This  field is read-only. */
	static var No(default, never):cs.system.componentmodel.RecommendedAsConfigurableAttribute;
	/** Specifies that a property can be used as an application setting. This  field is read-only. */
	static var Yes(default, never):cs.system.componentmodel.RecommendedAsConfigurableAttribute;
	/**
	 * Gets a value indicating whether the property this attribute is bound to can be
	 * used as an application setting.
	 * @return if the property this attribute is bound to can be used as an application
	 * setting; otherwise, .
	 */
	var RecommendedAsConfigurable(default, never):Bool;
	function new(recommendedAsConfigurable:Bool):Void;
	/**
	 * Indicates whether this instance and a specified object are equal.
	 * @param obj Another object to compare to.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Indicates whether the value of this instance is the default value for the class.
	 * @return if this instance is the default attribute for the class; otherwise, .
	 */
	function IsDefaultAttribute():Bool;
}
