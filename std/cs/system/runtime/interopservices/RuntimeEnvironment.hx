package cs.system.runtime.interopservices;

/** Provides a collection of  methods that return information about the common language runtime environment. */
@:native("System.Runtime.InteropServices.RuntimeEnvironment")
extern class RuntimeEnvironment {
	/**
	 * Gets the path to the system configuration file.
	 * @return The path to the system configuration file.
	 */
	static var SystemConfigurationFile(default, never):String;
	/**
	 * Tests whether the specified assembly is loaded in the global assembly cache.
	 * @param a The assembly to test.
	 * @return if the assembly is loaded in the global assembly cache; otherwise, .
	 */
	static function FromGlobalAccessCache(a:cs.system.reflection.Assembly):Bool;
	/**
	 * Returns the directory where the common language runtime is installed.
	 * @return A string that contains the path to the directory where the common
	 * language runtime is installed.
	 */
	static function GetRuntimeDirectory():String;
	/**
	 * Returns the specified interface on the specified class.
	 * @param clsid The identifier for the desired class.
	 * @param riid The identifier for the desired interface.
	 * @return An unmanaged pointer to the requested interface.
	 */
	static function GetRuntimeInterfaceAsIntPtr(clsid:cs.system.Guid, riid:cs.system.Guid):cs.system.IntPtr;
	/**
	 * Returns an instance of a type that represents a COM object by a pointer to its 
	 * interface.
	 * @param clsid The identifier for the desired class.
	 * @param riid The identifier for the desired interface.
	 * @return An object that represents the specified unmanaged COM object.
	 */
	static function GetRuntimeInterfaceAsObject(clsid:cs.system.Guid, riid:cs.system.Guid):Dynamic;
	/**
	 * Gets the version number of the common language runtime that is running the
	 * current process.
	 * @return A string containing the version number of the common language runtime.
	 */
	static function GetSystemVersion():String;
}
