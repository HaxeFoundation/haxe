package cs.system.componentmodel;

/** Specifies the installer for a type that installs components. */
@:native("System.ComponentModel.InstallerTypeAttribute")
extern class InstallerTypeAttribute extends cs.system.Attribute {
	/**
	 * Gets the type of installer associated with this attribute.
	 * @return A  that represents the type of installer associated with this attribute,
	 * or  if an installer does not exist.
	 */
	var InstallerType(default, never):cs.system.Type;
	@:overload(function(typeName:String):Void {})
	function new(installerType:cs.system.Type):Void;
	/**
	 * Returns whether the value of the given object is equal to the current .
	 * @param obj The object to test the value equality of.
	 * @return if the value of the given object is equal to that of the current;
	 * otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hashcode for this object.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
}
