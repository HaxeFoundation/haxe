package cs.system.diagnostics;

/** Represents a.dll or .exe file that is loaded into a particular process. */
@:native("System.Diagnostics.ProcessModule")
extern class ProcessModule extends cs.system.componentmodel.Component {
	/**
	 * Gets the memory address where the module was loaded.
	 * @return The load address of the module.
	 */
	var BaseAddress(default, never):cs.system.IntPtr;
	/**
	 * Gets the memory address for the function that runs when the system loads and
	 * runs the module.
	 * @return The entry point of the module.
	 */
	var EntryPointAddress(default, never):cs.system.IntPtr;
	/**
	 * Gets the full path to the module.
	 * @return The fully qualified path that defines the location of the module.
	 */
	var FileName(default, never):String;
	/**
	 * Gets version information about the module.
	 * @return A  that contains the module's version information.
	 */
	var FileVersionInfo(default, never):cs.system.diagnostics.FileVersionInfo;
	/**
	 * Gets the amount of memory that is required to load the module.
	 * @return The size, in bytes, of the memory that the module occupies.
	 */
	var ModuleMemorySize(default, never):Int;
	/**
	 * Gets the name of the process module.
	 * @return The name of the module.
	 */
	var ModuleName(default, never):String;
	/**
	 * Converts the name of the module to a string.
	 * @return The value of the  property.
	 */
	function ToString():String;
}
