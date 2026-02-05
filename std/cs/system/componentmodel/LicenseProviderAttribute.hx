package cs.system.componentmodel;

/** Specifies the  to use with a class. This class cannot be inherited. */
@:native("System.ComponentModel.LicenseProviderAttribute")
extern class LicenseProviderAttribute extends cs.system.Attribute {
	/** Specifies the default value, which is no provider. This  field is read-only. */
	static var Default(default, never):cs.system.componentmodel.LicenseProviderAttribute;
	/**
	 * Gets the license provider that must be used with the associated class.
	 * @return A  that represents the type of the license provider. The default value
	 * is .
	 */
	var LicenseProvider(default, never):cs.system.Type;
	@:overload(function():Void {})
	@:overload(function(typeName:String):Void {})
	function new(type:cs.system.Type):Void;
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
}
