package cs.system.runtime.interopservices;

/** Specifies the paths that are used to search for DLLs that provide functions for platform invokes. */
@:native("System.Runtime.InteropServices.DefaultDllImportSearchPathsAttribute")
extern class DefaultDllImportSearchPathsAttribute extends cs.system.Attribute {
	/**
	 * Gets a bitwise combination of enumeration values that specify the paths that the
	 * LoadLibraryEx function searches during platform invokes.
	 * @return A bitwise combination of enumeration values that specify search paths
	 * for platform invokes.
	 */
	var Paths(default, never):cs.system.runtime.interopservices.DllImportSearchPath;
	function new(paths:cs.system.runtime.interopservices.DllImportSearchPath):Void;
}
