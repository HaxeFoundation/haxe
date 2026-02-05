package cs.system.runtime.interopservices;

/** Specifies the version number of an exported type library. */
@:native("System.Runtime.InteropServices.TypeLibVersionAttribute")
extern class TypeLibVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets the major version number of the type library.
	 * @return The major version number of the type library.
	 */
	var MajorVersion(default, never):Int;
	/**
	 * Gets the minor version number of the type library.
	 * @return The minor version number of the type library.
	 */
	var MinorVersion(default, never):Int;
	function new(major:Int, minor:Int):Void;
}
