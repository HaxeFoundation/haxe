package cs.system;

/** Specifies the usage of another attribute class. This class cannot be inherited. */
@:native("System.AttributeUsageAttribute")
extern class AttributeUsageAttribute extends cs.system.Attribute {
	/**
	 * Gets or sets a Boolean value indicating whether more than one instance of the
	 * indicated attribute can be specified for a single program element.
	 * @return if more than one instance is allowed to be specified; otherwise, . The
	 * default is .
	 */
	var AllowMultiple(default, default):Bool;
	/**
	 * Gets or sets a  value that determines whether the indicated attribute is
	 * inherited by derived classes and overriding members.
	 * @return if the attribute can be inherited by derived classes and overriding
	 * members; otherwise, . The default is .
	 */
	var Inherited(default, default):Bool;
	/**
	 * Gets a set of values identifying which program elements that the indicated
	 * attribute can be applied to.
	 * @return One or several  values. The default is .
	 */
	var ValidOn(default, never):cs.system.AttributeTargets;
	function new(validOn:cs.system.AttributeTargets):Void;
}
