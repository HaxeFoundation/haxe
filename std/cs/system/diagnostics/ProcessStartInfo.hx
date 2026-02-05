package cs.system.diagnostics;

/** Specifies a set of values that are used when you start a process. */
@:native("System.Diagnostics.ProcessStartInfo")
extern class ProcessStartInfo {
	/**
	 * Gets a collection of command-line arguments to use when starting the
	 * application.
	 * @return A collection of command-line arguments.
	 */
	var ArgumentList(default, never):cs.system.collections.objectmodel.Collection<String>;
	/**
	 * Gets or sets the set of command-line arguments to use when starting the
	 * application.
	 * @return A single string containing the arguments to pass to the target
	 * application specified in the  property. The default is an empty string ("").
	 */
	var Arguments(default, default):String;
	/**
	 * Gets or sets a value indicating whether to start the process in a new window.
	 * @return if the process should be started without creating a new window to
	 * contain it; otherwise, . The default is .
	 */
	var CreateNoWindow(default, default):Bool;
	/**
	 * Gets or sets a value that identifies the domain to use when starting the
	 * process. If this value is , the  property must be specified in UPN format.
	 * @return The Active Directory domain to use when starting the process. If this
	 * value is , the  property must be specified in UPN format.
	 */
	var Domain(default, default):String;
	/**
	 * Gets the environment variables that apply to this process and its child
	 * processes.
	 * @return A generic dictionary containing the environment variables that apply to
	 * this process and its child processes. The default is .
	 */
	var Environment(default, never):cs.system.collections.generic.IDictionary<String, String>;
	/**
	 * Gets search paths for files, directories for temporary files,
	 * application-specific options, and other similar information.
	 * @return A string dictionary that provides environment variables that apply to
	 * this process and child processes. The default is .
	 */
	var EnvironmentVariables(default, never):cs.system.collections.specialized.StringDictionary;
	/**
	 * Gets or sets a value indicating whether an error dialog box is displayed to the
	 * user if the process cannot be started.
	 * @return if an error dialog box should be displayed on the screen if the process
	 * cannot be started; otherwise, . The default is .
	 */
	var ErrorDialog(default, default):Bool;
	/**
	 * Gets or sets the window handle to use when an error dialog box is shown for a
	 * process that cannot be started.
	 * @return A pointer to the handle of the error dialog box that results from a
	 * process start failure.
	 */
	var ErrorDialogParentHandle(default, default):cs.system.IntPtr;
	/**
	 * Gets or sets the application or document to start.
	 * @return The name of the application to start, or the name of a document of a
	 * file type that is associated with an application and that has a default open
	 * action available to it. The default is an empty string ("").
	 */
	var FileName(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the Windows user profile is to be
	 * loaded from the registry.
	 * @return if the Windows user profile should be loaded; otherwise, . The default
	 * is .
	 */
	var LoadUserProfile(default, default):Bool;
	/**
	 * Gets or sets a secure string that contains the user password to use when
	 * starting the process.
	 * @return The user password to use when starting the process.
	 */
	var Password(default, default):cs.system.security.SecureString;
	/**
	 * Gets or sets the user password in clear text to use when starting the process.
	 * @return The user password in clear text.
	 */
	var PasswordInClearText(default, default):String;
	/**
	 * Gets or sets a value that indicates whether the error output of an application
	 * is written to the  stream.
	 * @return if error output should be written to ; otherwise, . The default is .
	 */
	var RedirectStandardError(default, default):Bool;
	/**
	 * Gets or sets a value indicating whether the input for an application is read
	 * from the  stream.
	 * @return if input should be read from ; otherwise, . The default is .
	 */
	var RedirectStandardInput(default, default):Bool;
	/**
	 * Gets or sets a value that indicates whether the textual output of an application
	 * is written to the  stream.
	 * @return if output should be written to ; otherwise, . The default is .
	 */
	var RedirectStandardOutput(default, default):Bool;
	/**
	 * Gets or sets the preferred encoding for error output.
	 * @return An object that represents the preferred encoding for error output. The
	 * default is .
	 */
	var StandardErrorEncoding(default, default):cs.system.text.Encoding;
	var StandardInputEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the preferred encoding for standard output.
	 * @return An object that represents the preferred encoding for standard output.
	 * The default is .
	 */
	var StandardOutputEncoding(default, default):cs.system.text.Encoding;
	/**
	 * Gets or sets the user name to use when starting the process. If you use the UPN
	 * format, @, the  property must be .
	 * @return The user name to use when starting the process. If you use the UPN
	 * format, @, the  property must be .
	 */
	var UserName(default, default):String;
	/**
	 * Gets or sets a value indicating whether to use the operating system shell to
	 * start the process.
	 * @return if the shell should be used when starting the process;  if the process
	 * should be created directly from the executable file. The default is  on .NET
	 * Framework apps and  on .NET Core apps.
	 */
	var UseShellExecute(default, default):Bool;
	/**
	 * Gets or sets the verb to use when opening the application or document specified
	 * by the  property.
	 * @return The action to take with the file that the process opens. The default is
	 * an empty string (""), which signifies no action.
	 */
	var Verb(default, default):String;
	/**
	 * Gets the set of verbs associated with the type of file specified by the 
	 * property.
	 * @return The actions that the system can apply to the file indicated by the 
	 * property.
	 */
	var Verbs(default, never):cs.NativeArray<String>;
	/**
	 * Gets or sets the window state to use when the process is started.
	 * @return One of the enumeration values that indicates whether the process is
	 * started in a window that is maximized, minimized, normal (neither maximized nor
	 * minimized), or not visible. The default is .
	 */
	var WindowStyle(default, default):cs.system.diagnostics.ProcessWindowStyle;
	/**
	 * When the  property is , gets or sets the working directory for the process to be
	 * started. When  is , gets or sets the directory that contains the process to be
	 * started.
	 * @return When  is , the fully qualified name of the directory that contains the
	 * process to be started. When the  property is , the working directory for the
	 * process to be started. The default is an empty string ("").
	 */
	var WorkingDirectory(default, default):String;
	@:overload(function():Void {})
	@:overload(function(fileName:String):Void {})
	function new(fileName:String, arguments:String):Void;
}
