package cs.system.runtime.interopservices;

/** Indicates that the attributed assembly is a primary interop assembly. */
@:native("System.Runtime.InteropServices.PrimaryInteropAssemblyAttribute")
extern class PrimaryInteropAssemblyAttribute extends cs.system.Attribute {
	/**
	 * Gets the major version number of the type library for which this assembly is the
	 * primary interop assembly.
	 * @return The major version number of the type library for which this assembly is
	 * the primary interop assembly.
	 */
	var MajorVersion(default, never):Int;
	/**
	 * Gets the minor version number of the type library for which this assembly is the
	 * primary interop assembly.
	 * @return The minor version number of the type library for which this assembly is
	 * the primary interop assembly.
	 */
	var MinorVersion(default, never):Int;
	function new(major:Int, minor:Int):Void;
}
