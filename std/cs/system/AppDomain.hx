package cs.system;

/** Represents an application domain, which is an isolated environment where applications execute. This class cannot be inherited. */
@:native("System.AppDomain")
extern class AppDomain extends cs.system.MarshalByRefObject {
	/**
	 * Gets the current application domain for the current .
	 * @return The current application domain.
	 */
	static var CurrentDomain(default, never):cs.system.AppDomain;
	/**
	 * Gets or sets a value that indicates whether CPU and memory monitoring of
	 * application domains is enabled for the current process. Once monitoring is
	 * enabled for a process, it cannot be disabled.
	 * @return if monitoring is enabled; otherwise .
	 */
	static var MonitoringIsEnabled(default, default):Bool;
	/**
	 * Gets the total bytes that survived from the last collection for all application
	 * domains in the process.
	 * @return The total number of surviving bytes for the process.
	 */
	static var MonitoringSurvivedProcessMemorySize(default, never):haxe.Int64;
	/**
	 * Gets the base directory that the assembly resolver uses to probe for assemblies.
	 * @return The base directory that the assembly resolver uses to probe for
	 * assemblies.
	 */
	var BaseDirectory(default, never):String;
	/**
	 * Gets the directory that the assembly resolver uses to probe for dynamically
	 * created assemblies.
	 * @return The directory that the assembly resolver uses to probe for dynamically
	 * created assemblies.
	 */
	var DynamicDirectory(default, never):String;
	/**
	 * Gets the friendly name of this application domain.
	 * @return The friendly name of this application domain.
	 */
	var FriendlyName(default, never):String;
	/**
	 * Gets an integer that uniquely identifies the application domain within the
	 * process.
	 * @return An integer that identifies the application domain.
	 */
	var Id(default, never):Int;
	/**
	 * Gets a value that indicates whether assemblies that are loaded into the current
	 * application domain execute with full trust.
	 * @return if assemblies that are loaded into the current application domain
	 * execute with full trust; otherwise, .
	 */
	var IsFullyTrusted(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current application domain has a set of
	 * permissions that is granted to all assemblies that are loaded into the
	 * application domain.
	 * @return if the current application domain has a homogenous set of permissions;
	 * otherwise, .
	 */
	var IsHomogenous(default, never):Bool;
	/**
	 * Gets the number of bytes that survived the last collection and that are known to
	 * be referenced by the current application domain.
	 * @return The number of surviving bytes.
	 */
	var MonitoringSurvivedMemorySize(default, never):haxe.Int64;
	/**
	 * Gets the total size, in bytes, of all memory allocations that have been made by
	 * the application domain since it was created, without subtracting memory that has
	 * been collected.
	 * @return The total size of all memory allocations.
	 */
	var MonitoringTotalAllocatedMemorySize(default, never):haxe.Int64;
	/**
	 * Gets the total processor time that has been used by all threads while executing
	 * in the current application domain, since the process started.
	 * @return Total processor time for the current application domain.
	 */
	var MonitoringTotalProcessorTime(default, never):cs.system.TimeSpan;
	/**
	 * Gets the path under the base directory where the assembly resolver should probe
	 * for private assemblies.
	 * @return The path under the base directory where the assembly resolver should
	 * probe for private assemblies.
	 */
	var RelativeSearchPath(default, never):String;
	/**
	 * Gets an indication whether the application domain is configured to shadow copy
	 * files.
	 * @return if the application domain is configured to shadow copy files; otherwise,
	 * .
	 */
	var ShadowCopyFiles(default, never):Bool;
	/**
	 * Creates a new application domain with the specified name.
	 * @param friendlyName The friendly name of the domain.
	 * @return The newly created application domain.
	 */
	static function CreateDomain(friendlyName:String):cs.system.AppDomain;
	/**
	 * Gets the current thread identifier.
	 * @return A 32-bit signed integer that is the identifier of the current thread.
	 */
	static function GetCurrentThreadId():Int;
	/**
	 * Unloads the specified application domain.
	 * @param domain An application domain to unload.
	 */
	static function Unload(domain:cs.system.AppDomain):Void;
	/**
	 * Appends the specified directory name to the private path list.
	 * @param path The name of the directory to be appended to the private path.
	 */
	function AppendPrivatePath(path:String):Void;
	/**
	 * Returns the assembly display name after policy has been applied.
	 * @param assemblyName The assembly display name, in the form provided by the 
	 * property.
	 * @return A string containing the assembly display name after policy has been
	 * applied.
	 */
	function ApplyPolicy(assemblyName:String):String;
	/** Resets the path that specifies the location of private assemblies to the empty string (""). */
	function ClearPrivatePath():Void;
	/** Resets the list of directories containing shadow copied assemblies to the empty string (""). */
	function ClearShadowCopyPath():Void;
	@:overload(function(assemblyFile:String):Int {})
	@:overload(function(assemblyFile:String, args:cs.NativeArray<String>):Int {})
	/**
	 * Executes the assembly contained in the specified file.
	 * @param assemblyFile The name of the file that contains the assembly to execute.
	 * @return The value returned by the entry point of the assembly.
	 */
	function ExecuteAssembly(assemblyFile:String, args:cs.NativeArray<String>, hashValue:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.configuration.assemblies.AssemblyHashAlgorithm):Int;
	@:overload(function(assemblyName:String):Int {})
	@:overload(function(assemblyName:cs.system.reflection.AssemblyName, args:cs.NativeArray<String>):Int {})
	/**
	 * Executes the assembly given an , using the specified arguments.
	 * @param assemblyName An  object representing the name of the assembly.
	 * @param args Command-line arguments to pass when starting the process.
	 * @return The value that is returned by the entry point of the assembly.
	 */
	function ExecuteAssemblyByName(assemblyName:String, args:cs.NativeArray<String>):Int;
	/**
	 * Gets the assemblies that have been loaded into the execution context of this
	 * application domain.
	 * @return An array of assemblies in this application domain.
	 */
	function GetAssemblies():cs.NativeArray<cs.system.reflection.Assembly>;
	/**
	 * Gets the value stored in the current application domain for the specified name.
	 * @param name The name of a predefined application domain property, or the name of
	 * an application domain property you have defined.
	 * @return The value of the  property, or  if the property does not exist.
	 */
	function GetData(name:String):Dynamic;
	/**
	 * Gets a nullable Boolean value that indicates whether any compatibility switches
	 * are set, and if so, whether the specified compatibility switch is set.
	 * @param value The compatibility switch to test.
	 * @return A null reference ( in Visual Basic) if no compatibility switches are
	 * set; otherwise, a Boolean value that indicates whether the compatibility switch
	 * that is specified by  is set.
	 */
	function IsCompatibilitySwitchSet(value:String):Null<Bool>;
	/**
	 * Returns a value that indicates whether the application domain is the default
	 * application domain for the process.
	 * @return if the current  object represents the default application domain for the
	 * process; otherwise, .
	 */
	function IsDefaultAppDomain():Bool;
	/**
	 * Indicates whether this application domain is unloading, and the objects it
	 * contains are being finalized by the common language runtime.
	 * @return if this application domain is unloading and the common language runtime
	 * has started invoking finalizers; otherwise, .
	 */
	function IsFinalizingForUnload():Bool;
	@:overload(function(rawAssembly:cs.NativeArray<cs.UInt8>):cs.system.reflection.Assembly {})
	@:overload(function(assemblyRef:cs.system.reflection.AssemblyName):cs.system.reflection.Assembly {})
	@:overload(function(assemblyString:String):cs.system.reflection.Assembly {})
	/**
	 * Loads the  with a common object file format (COFF) based image containing an
	 * emitted .
	 * @param rawAssembly An array of type  that is a COFF-based image containing an
	 * emitted assembly.
	 * @return The loaded assembly.
	 */
	function Load(rawAssembly:cs.NativeArray<cs.UInt8>, rawSymbolStore:cs.NativeArray<cs.UInt8>):cs.system.reflection.Assembly;
	/**
	 * Returns the assemblies that have been loaded into the reflection-only context of
	 * the application domain.
	 * @return An array of  objects that represent the assemblies loaded into the
	 * reflection-only context of the application domain.
	 */
	function ReflectionOnlyGetAssemblies():cs.NativeArray<cs.system.reflection.Assembly>;
	/**
	 * Establishes the specified directory path as the location where assemblies are
	 * shadow copied.
	 * @param path The fully qualified path to the shadow copy location.
	 */
	function SetCachePath(path:String):Void;
	/**
	 * Assigns the specified value to the specified application domain property.
	 * @param name The name of a user-defined application domain property to create or
	 * change.
	 * @param data The value of the property.
	 */
	function SetData(name:String, data:Dynamic):Void;
	/**
	 * Establishes the specified directory path as the base directory for
	 * subdirectories where dynamically generated files are stored and accessed.
	 * @param path The fully qualified path that is the base directory for
	 * subdirectories where dynamic assemblies are stored.
	 */
	function SetDynamicBase(path:String):Void;
	/**
	 * Specifies how principal and identity objects should be attached to a thread if
	 * the thread attempts to bind to a principal while executing in this application
	 * domain.
	 * @param policy One of the  values that specifies the type of the principal object
	 * to attach to threads.
	 */
	function SetPrincipalPolicy(policy:cs.system.security.principal.PrincipalPolicy):Void;
	/** Turns on shadow copying. */
	function SetShadowCopyFiles():Void;
	/**
	 * Establishes the specified directory path as the location of assemblies to be
	 * shadow copied.
	 * @param path A list of directory names, where each name is separated by a
	 * semicolon.
	 */
	function SetShadowCopyPath(path:String):Void;
	/**
	 * Sets the default principal object to be attached to threads if they attempt to
	 * bind to a principal while executing in this application domain.
	 * @param principal The principal object to attach to threads.
	 */
	function SetThreadPrincipal(principal:cs.system.security.principal.IPrincipal):Void;
	/**
	 * Obtains a string representation that includes the friendly name of the
	 * application domain and any context policies.
	 * @return A string formed by concatenating the literal string "Name:", the
	 * friendly name of the application domain, and either string representations of
	 * the context policies or the string "There are no context policies."
	 */
	function ToString():String;
}
