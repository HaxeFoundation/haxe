package cs.system;

/** Provides information about, and means to manipulate, the current environment and platform. This class cannot be inherited. */
@:native("System.Environment")
extern class Environment {
	/**
	 * Gets the command line for this process.
	 * @return A string containing command-line arguments.
	 */
	static var CommandLine(default, never):String;
	/**
	 * Gets or sets the fully qualified path of the current working directory.
	 * @return A string containing a directory path.
	 */
	static var CurrentDirectory(default, default):String;
	/**
	 * Gets a unique identifier for the current managed thread.
	 * @return An integer that represents a unique identifier for this managed thread.
	 */
	static var CurrentManagedThreadId(default, never):Int;
	/**
	 * Gets or sets the exit code of the process.
	 * @return A 32-bit signed integer containing the exit code. The default value is 0
	 * (zero), which indicates that the process completed successfully.
	 */
	static var ExitCode(default, default):Int;
	/**
	 * Gets a value that indicates whether the current application domain is being
	 * unloaded or the common language runtime (CLR) is shutting down.
	 * @return if the current application domain is being unloaded or the CLR is
	 * shutting down; otherwise, .
	 */
	static var HasShutdownStarted(default, never):Bool;
	/**
	 * Determines whether the current operating system is a 64-bit operating system.
	 * @return if the operating system is 64-bit; otherwise, .
	 */
	static var Is64BitOperatingSystem(default, never):Bool;
	/**
	 * Determines whether the current process is a 64-bit process.
	 * @return if the process is 64-bit; otherwise, .
	 */
	static var Is64BitProcess(default, never):Bool;
	/**
	 * Gets the NetBIOS name of this local computer.
	 * @return A string containing the name of this computer.
	 */
	static var MachineName(default, never):String;
	/**
	 * Gets the newline string defined for this environment.
	 * @return A string containing "\r\n" for non-Unix platforms, or a string
	 * containing "\n" for Unix platforms.
	 */
	static var NewLine(default, never):String;
	/**
	 * Gets an  object that contains the current platform identifier and version
	 * number.
	 * @return An object that contains the platform identifier and version number.
	 */
	static var OSVersion(default, never):cs.system.OperatingSystem;
	/**
	 * Gets the number of processors on the current machine.
	 * @return The 32-bit signed integer that specifies the number of processors on the
	 * current machine. There is no default. If the current machine contains multiple
	 * processor groups, this property returns the number of logical processors that
	 * are available for use by the common language runtime (CLR).
	 */
	static var ProcessorCount(default, never):Int;
	/**
	 * Gets current stack trace information.
	 * @return A string containing stack trace information. This value can be .
	 */
	static var StackTrace(default, never):String;
	/**
	 * Gets the fully qualified path of the system directory.
	 * @return A string containing a directory path.
	 */
	static var SystemDirectory(default, never):String;
	/**
	 * Gets the number of bytes in the operating system's memory page.
	 * @return The number of bytes in the system memory page.
	 */
	static var SystemPageSize(default, never):Int;
	/**
	 * Gets the number of milliseconds elapsed since the system started.
	 * @return A 32-bit signed integer containing the amount of time in milliseconds
	 * that has passed since the last time the computer was started.
	 */
	static var TickCount(default, never):Int;
	/**
	 * Gets the network domain name associated with the current user.
	 * @return The network domain name associated with the current user.
	 */
	static var UserDomainName(default, never):String;
	/**
	 * Gets a value indicating whether the current process is running in user
	 * interactive mode.
	 * @return if the current process is running in user interactive mode; otherwise, .
	 */
	static var UserInteractive(default, never):Bool;
	/**
	 * Gets the user name of the person who is currently logged on to the operating
	 * system.
	 * @return The user name of the person who is logged on to the operating system.
	 */
	static var UserName(default, never):String;
	/**
	 * Gets a  object that describes the major, minor, build, and revision numbers of
	 * the common language runtime.
	 * @return An object that displays the version of the common language runtime.
	 */
	static var Version(default, never):cs.system.Version;
	/**
	 * Gets the amount of physical memory mapped to the process context.
	 * @return A 64-bit signed integer containing the number of bytes of physical
	 * memory mapped to the process context.
	 */
	static var WorkingSet(default, never):haxe.Int64;
	/**
	 * Terminates this process and returns an exit code to the operating system.
	 * @param exitCode The exit code to return to the operating system. Use 0 (zero) to
	 * indicate that the process completed successfully.
	 */
	static function Exit(exitCode:Int):Void;
	/**
	 * Replaces the name of each environment variable embedded in the specified string
	 * with the string equivalent of the value of the variable, then returns the
	 * resulting string.
	 * @param name A string containing the names of zero or more environment variables.
	 * Each environment variable is quoted with the percent sign character (%).
	 * @return A string with each environment variable replaced by its value.
	 */
	static function ExpandEnvironmentVariables(name:String):String;
	@:overload(function(message:String):Void {})
	/**
	 * Immediately terminates a process after writing a message to the Windows
	 * Application event log, and then includes the message in error reporting to
	 * Microsoft.
	 * @param message A message that explains why the process was terminated, or  if no
	 * explanation is provided.
	 */
	static function FailFast(message:String, exception:cs.system.Exception):Void;
	/**
	 * Returns a string array containing the command-line arguments for the current
	 * process.
	 * @return An array of string where each element contains a command-line argument.
	 * The first element is the executable file name, and the following zero or more
	 * elements contain the remaining command-line arguments.
	 */
	static function GetCommandLineArgs():cs.NativeArray<String>;
	@:overload(function(variable:String):String {})
	/**
	 * Retrieves the value of an environment variable from the current process.
	 * @param variable The name of the environment variable.
	 * @return The value of the environment variable specified by , or  if the
	 * environment variable is not found.
	 */
	static function GetEnvironmentVariable(variable:String, target:cs.system.EnvironmentVariableTarget):String;
	@:overload(function():cs.system.collections.IDictionary {})
	/**
	 * Retrieves all environment variable names and their values from the current
	 * process.
	 * @return A dictionary that contains all environment variable names and their
	 * values; otherwise, an empty dictionary if no environment variables are found.
	 */
	static function GetEnvironmentVariables(target:cs.system.EnvironmentVariableTarget):cs.system.collections.IDictionary;
	@:overload(function(folder:cs.system.Environment_SpecialFolder):String {})
	/**
	 * Gets the path to the system special folder that is identified by the specified
	 * enumeration.
	 * @param folder One of enumeration values that identifies a system special folder.
	 * @return The path to the specified system special folder, if that folder
	 * physically exists on your computer; otherwise, an empty string (""). A folder
	 * will not physically exist if the operating system did not create it, the
	 * existing folder was deleted, or the folder is a virtual directory, such as My
	 * Computer, which does not correspond to a physical path.
	 */
	static function GetFolderPath(folder:cs.system.Environment_SpecialFolder, option:cs.system.Environment_SpecialFolderOption):String;
	/**
	 * Returns an array of string containing the names of the logical drives on the
	 * current computer.
	 * @return An array of strings where each element contains the name of a logical
	 * drive. For example, if the computer's hard drive is the first logical drive, the
	 * first element returned is "C:\".
	 */
	static function GetLogicalDrives():cs.NativeArray<String>;
	@:overload(function(variable:String, value:String):Void {})
	/**
	 * Creates, modifies, or deletes an environment variable stored in the current
	 * process.
	 * @param variable The name of an environment variable.
	 * @param value A value to assign to .
	 */
	static function SetEnvironmentVariable(variable:String, value:String, target:cs.system.EnvironmentVariableTarget):Void;
}
