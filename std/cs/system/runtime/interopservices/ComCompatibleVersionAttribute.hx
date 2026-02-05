package cs.system.runtime.interopservices;

/** Indicates to a COM client that all classes in the current version of an assembly are compatible with classes in an earlier version of the assembly. */
@:native("System.Runtime.InteropServices.ComCompatibleVersionAttribute")
extern class ComCompatibleVersionAttribute extends cs.system.Attribute {
	/**
	 * Gets the build number of the assembly.
	 * @return The build number of the assembly.
	 */
	var BuildNumber(default, never):Int;
	/**
	 * Gets the major version number of the assembly.
	 * @return The major version number of the assembly.
	 */
	var MajorVersion(default, never):Int;
	/**
	 * Gets the minor version number of the assembly.
	 * @return The minor version number of the assembly.
	 */
	var MinorVersion(default, never):Int;
	/**
	 * Gets the revision number of the assembly.
	 * @return The revision number of the assembly.
	 */
	var RevisionNumber(default, never):Int;
	function new(major:Int, minor:Int, build:Int, revision:Int):Void;
}
