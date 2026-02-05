package cs.system.runtime.interopservices;

/** Provides information about the .NET runtime installation. */
@:native("System.Runtime.InteropServices.RuntimeInformation")
extern class RuntimeInformation {
	/**
	 * Returns a string that indicates the name of the .NET installation on which an
	 * app is running.
	 * @return The name of the .NET installation on which the app is running.
	 */
	static var FrameworkDescription(default, never):String;
	/**
	 * Gets the platform architecture on which the current app is running.
	 * @return The platform architecture on which the current app is running.
	 */
	static var OSArchitecture(default, never):cs.system.runtime.interopservices.Architecture;
	/**
	 * Gets a string that describes the operating system on which the app is running.
	 * @return The description of the operating system on which the app is running.
	 */
	static var OSDescription(default, never):String;
	/**
	 * Gets the process architecture of the currently running app.
	 * @return The process architecture of the currently running app.
	 */
	static var ProcessArchitecture(default, never):cs.system.runtime.interopservices.Architecture;
	/**
	 * Indicates whether the current application is running on the specified platform.
	 * @param osPlatform A platform.
	 * @return if the current app is running on the specified platform; otherwise, .
	 */
	static function IsOSPlatform(osPlatform:cs.system.runtime.interopservices.OSPlatform):Bool;
}
