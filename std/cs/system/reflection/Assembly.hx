package cs.system.reflection;

/** Represents an assembly, which is a reusable, versionable, and self-describing building block of a common language runtime application. */
@:native("System.Reflection.Assembly")
extern class Assembly {
	/**
	 * Gets the location of the assembly as specified originally, for example, in an 
	 * object.
	 * @return The location of the assembly as specified originally.
	 */
	var CodeBase(default, never):String;
	/**
	 * Gets a collection that contains this assembly's custom attributes.
	 * @return A collection that contains this assembly's custom attributes.
	 */
	var CustomAttributes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets a collection of the types defined in this assembly.
	 * @return A collection of the types defined in this assembly.
	 */
	var DefinedTypes(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.TypeInfo>;
	/**
	 * Gets the entry point of this assembly.
	 * @return An object that represents the entry point of this assembly. If no entry
	 * point is found (for example, the assembly is a DLL),  is returned.
	 */
	var EntryPoint(default, never):cs.system.reflection.MethodInfo;
	/**
	 * Gets the URI, including escape characters, that represents the codebase.
	 * @return A URI with escape characters.
	 */
	var EscapedCodeBase(default, never):String;
	/**
	 * Gets a collection of the public types defined in this assembly that are visible
	 * outside the assembly.
	 * @return A collection of the public types defined in this assembly that are
	 * visible outside the assembly.
	 */
	var ExportedTypes(default, never):cs.system.collections.generic.IEnumerable<cs.system.Type>;
	/**
	 * Gets the display name of the assembly.
	 * @return The display name of the assembly.
	 */
	var FullName(default, never):String;
	/**
	 * Gets a value indicating whether the assembly was loaded from the global assembly
	 * cache.
	 * @return if the assembly was loaded from the global assembly cache; otherwise, .
	 */
	var GlobalAssemblyCache(default, never):Bool;
	/**
	 * Gets the host context with which the assembly was loaded.
	 * @return An  value that indicates the host context with which the assembly was
	 * loaded, if any.
	 */
	var HostContext(default, never):haxe.Int64;
	/**
	 * Gets a string representing the version of the common language runtime (CLR)
	 * saved in the file containing the manifest.
	 * @return The CLR version folder name. This is not a full path.
	 */
	var ImageRuntimeVersion(default, never):String;
	/**
	 * Gets a value that indicates whether the current assembly was generated
	 * dynamically in the current process by using reflection emit.
	 * @return if the current assembly was generated dynamically in the current
	 * process; otherwise, .
	 */
	var IsDynamic(default, never):Bool;
	/**
	 * Gets a value that indicates whether the current assembly is loaded with full
	 * trust.
	 * @return if the current assembly is loaded with full trust; otherwise, .
	 */
	var IsFullyTrusted(default, never):Bool;
	/**
	 * Gets the full path or UNC location of the loaded file that contains the
	 * manifest.
	 * @return The location of the loaded file that contains the manifest. If the
	 * loaded file was shadow-copied, the location is that of the file after being
	 * shadow-copied. If the assembly is loaded from a byte array, such as when using
	 * the  method overload, the value returned is an empty string ("").
	 */
	var Location(default, never):String;
	/**
	 * Gets the module that contains the manifest for the current assembly.
	 * @return The module that contains the manifest for the assembly.
	 */
	var ManifestModule(default, never):cs.system.reflection.Module;
	/**
	 * Gets a collection that contains the modules in this assembly.
	 * @return A collection that contains the modules in this assembly.
	 */
	var Modules(default, never):cs.system.collections.generic.IEnumerable<cs.system.reflection.Module>;
	/**
	 * Gets a  value indicating whether this assembly was loaded into the
	 * reflection-only context.
	 * @return if the assembly was loaded into the reflection-only context, rather than
	 * the execution context; otherwise, .
	 */
	var ReflectionOnly(default, never):Bool;
	/**
	 * Gets a value that indicates which set of security rules the common language
	 * runtime (CLR) enforces for this assembly.
	 * @return The security rule set that the CLR enforces for this assembly.
	 */
	var SecurityRuleSet(default, never):cs.system.security.SecurityRuleSet;
	/**
	 * Creates the name of a type qualified by the display name of its assembly.
	 * @param assemblyName The display name of an assembly.
	 * @param typeName The full name of a type.
	 * @return The full name of the type qualified by the display name of the assembly.
	 */
	static function CreateQualifiedName(assemblyName:String, typeName:String):String;
	/**
	 * Gets the currently loaded assembly in which the specified type is defined.
	 * @param type An object representing a type in the assembly that will be returned.
	 * @return The assembly in which the specified type is defined.
	 */
	static function GetAssembly(type:cs.system.Type):cs.system.reflection.Assembly;
	/**
	 * Returns the  of the method that invoked the currently executing method.
	 * @return The  object of the method that invoked the currently executing method.
	 */
	static function GetCallingAssembly():cs.system.reflection.Assembly;
	/**
	 * Gets the process executable in the default application domain. In other
	 * application domains, this is the first executable that was executed by .
	 * @return The assembly that is the process executable in the default application
	 * domain, or the first executable that was executed by . Can return  when called
	 * from unmanaged code.
	 */
	static function GetEntryAssembly():cs.system.reflection.Assembly;
	/**
	 * Gets the assembly that contains the code that is currently executing.
	 * @return The assembly that contains the code that is currently executing.
	 */
	static function GetExecutingAssembly():cs.system.reflection.Assembly;
	@:overload(function(rawAssembly:cs.NativeArray<cs.UInt8>):cs.system.reflection.Assembly {})
	@:overload(function(assemblyRef:cs.system.reflection.AssemblyName):cs.system.reflection.Assembly {})
	@:overload(function(assemblyString:String):cs.system.reflection.Assembly {})
	/**
	 * Loads the assembly with a common object file format (COFF)-based image
	 * containing an emitted assembly. The assembly is loaded into the application
	 * domain of the caller.
	 * @param rawAssembly A byte array that is a COFF-based image containing an emitted
	 * assembly.
	 * @return The loaded assembly.
	 */
	static function Load(rawAssembly:cs.NativeArray<cs.UInt8>, rawSymbolStore:cs.NativeArray<cs.UInt8>):cs.system.reflection.Assembly;
	/**
	 * Loads the contents of an assembly file on the specified path.
	 * @param path The fully qualified path of the file to load.
	 * @return The loaded assembly.
	 */
	static function LoadFile(path:String):cs.system.reflection.Assembly;
	@:overload(function(assemblyFile:String):cs.system.reflection.Assembly {})
	/**
	 * Loads an assembly given its file name or path.
	 * @param assemblyFile The name or path of the file that contains the manifest of
	 * the assembly.
	 * @return The loaded assembly.
	 */
	static function LoadFrom(assemblyFile:String, hashValue:cs.NativeArray<cs.UInt8>, hashAlgorithm:cs.system.configuration.assemblies.AssemblyHashAlgorithm):cs.system.reflection.Assembly;
	/**
	 * Loads an assembly from the application directory or from the global assembly
	 * cache using a partial name.
	 * @param partialName The display name of the assembly.
	 * @return The loaded assembly. If  is not found, this method returns .
	 */
	static function LoadWithPartialName(partialName:String):cs.system.reflection.Assembly;
	/**
	 * Indicates whether two  objects are equal.
	 * @param left The assembly to compare to .
	 * @param right The assembly to compare to .
	 * @return if  is equal to ; otherwise, .
	 */
	static function op_Equality(left:cs.system.reflection.Assembly, right:cs.system.reflection.Assembly):Bool;
	/**
	 * Indicates whether two  objects are not equal.
	 * @param left The assembly to compare to .
	 * @param right The assembly to compare to .
	 * @return if  is not equal to ; otherwise, .
	 */
	static function op_Inequality(left:cs.system.reflection.Assembly, right:cs.system.reflection.Assembly):Bool;
	@:overload(function(rawAssembly:cs.NativeArray<cs.UInt8>):cs.system.reflection.Assembly {})
	/**
	 * Loads the assembly from a common object file format (COFF)-based image
	 * containing an emitted assembly. The assembly is loaded into the reflection-only
	 * context of the caller's application domain.
	 * @param rawAssembly A byte array that is a COFF-based image containing an emitted
	 * assembly.
	 * @return The loaded assembly.
	 */
	static function ReflectionOnlyLoad(assemblyString:String):cs.system.reflection.Assembly;
	/**
	 * Loads an assembly into the reflection-only context, given its path.
	 * @param assemblyFile The path of the file that contains the manifest of the
	 * assembly.
	 * @return The loaded assembly.
	 */
	static function ReflectionOnlyLoadFrom(assemblyFile:String):cs.system.reflection.Assembly;
	/**
	 * Loads an assembly into the load-from context, bypassing some security checks.
	 * @param assemblyFile The name or path of the file that contains the manifest of
	 * the assembly.
	 * @return The loaded assembly.
	 */
	static function UnsafeLoadFrom(assemblyFile:String):cs.system.reflection.Assembly;
	@:overload(function(typeName:String):Dynamic {})
	@:overload(function(typeName:String, ignoreCase:Bool):Dynamic {})
	/**
	 * Locates the specified type from this assembly and creates an instance of it
	 * using the system activator, using case-sensitive search.
	 * @param typeName The  of the type to locate.
	 * @return An instance of the specified type created with the parameterless
	 * constructor; or  if  is not found. The type is resolved using the default
	 * binder, without specifying culture or activation attributes, and with  set to 
	 * or .
	 */
	function CreateInstance(typeName:String, ignoreCase:Bool, bindingAttr:cs.system.reflection.BindingFlags, binder:cs.system.reflection.Binder, args:cs.NativeArray<Dynamic>, culture:cs.system.globalization.CultureInfo, activationAttributes:cs.NativeArray<Dynamic>):Dynamic;
	/**
	 * Determines whether this assembly and the specified object are equal.
	 * @param o The object to compare with this instance.
	 * @return if  is equal to this instance; otherwise, .
	 */
	function Equals(o:Dynamic):Bool;
	@:overload(function(inherit:Bool):cs.NativeArray<Dynamic> {})
	/**
	 * Gets all the custom attributes for this assembly.
	 * @param inherit This argument is ignored for objects of type .
	 * @return An array that contains the custom attributes for this assembly.
	 */
	function GetCustomAttributes(attributeType:cs.system.Type, inherit:Bool):cs.NativeArray<Dynamic>;
	/**
	 * Returns information about the attributes that have been applied to the current ,
	 * expressed as  objects.
	 * @return A generic list of  objects representing data about the attributes that
	 * have been applied to the current assembly.
	 */
	function GetCustomAttributesData():cs.system.collections.generic.IList<cs.system.reflection.CustomAttributeData>;
	/**
	 * Gets the public types defined in this assembly that are visible outside the
	 * assembly.
	 * @return An array that represents the types defined in this assembly that are
	 * visible outside the assembly.
	 */
	function GetExportedTypes():cs.NativeArray<cs.system.Type>;
	/**
	 * Gets a  for the specified file in the file table of the manifest of this
	 * assembly.
	 * @param name The name of the specified file. Do not include the path to the file.
	 * @return A stream that contains the specified file, or  if the file is not found.
	 */
	function GetFile(name:String):cs.system.io.FileStream;
	@:overload(function():cs.NativeArray<cs.system.io.FileStream> {})
	/**
	 * Gets the files in the file table of an assembly manifest.
	 * @return An array of streams that contain the files.
	 */
	function GetFiles(getResourceModules:Bool):cs.NativeArray<cs.system.io.FileStream>;
	function GetForwardedTypes():cs.NativeArray<cs.system.Type>;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	@:overload(function():cs.NativeArray<cs.system.reflection.Module> {})
	/**
	 * Gets all the loaded modules that are part of this assembly.
	 * @return An array of modules.
	 */
	function GetLoadedModules(getResourceModules:Bool):cs.NativeArray<cs.system.reflection.Module>;
	/**
	 * Returns information about how the given resource has been persisted.
	 * @param resourceName The case-sensitive name of the resource.
	 * @return An object that is populated with information about the resource's
	 * topology, or  if the resource is not found.
	 */
	function GetManifestResourceInfo(resourceName:String):cs.system.reflection.ManifestResourceInfo;
	/**
	 * Returns the names of all the resources in this assembly.
	 * @return An array that contains the names of all the resources.
	 */
	function GetManifestResourceNames():cs.NativeArray<String>;
	@:overload(function(name:String):cs.system.io.Stream {})
	/**
	 * Loads the specified manifest resource from this assembly.
	 * @param name The case-sensitive name of the manifest resource being requested.
	 * @return The manifest resource; or  if no resources were specified during
	 * compilation or if the resource is not visible to the caller.
	 */
	function GetManifestResourceStream(type:cs.system.Type, name:String):cs.system.io.Stream;
	/**
	 * Gets the specified module in this assembly.
	 * @param name The name of the module being requested.
	 * @return The module being requested, or  if the module is not found.
	 */
	function GetModule(name:String):cs.system.reflection.Module;
	@:overload(function():cs.NativeArray<cs.system.reflection.Module> {})
	/**
	 * Gets all the modules that are part of this assembly.
	 * @return An array of modules.
	 */
	function GetModules(getResourceModules:Bool):cs.NativeArray<cs.system.reflection.Module>;
	@:overload(function():cs.system.reflection.AssemblyName {})
	/**
	 * Gets an  for this assembly.
	 * @return An object that contains the fully parsed display name for this assembly.
	 */
	function GetName(copiedName:Bool):cs.system.reflection.AssemblyName;
	/**
	 * Gets serialization information with all of the data needed to reinstantiate this
	 * assembly.
	 * @param info The object to be populated with serialization information.
	 * @param context The destination context of the serialization.
	 */
	function GetObjectData(info:cs.system.runtime.serialization.SerializationInfo, context:cs.system.runtime.serialization.StreamingContext):Void;
	/**
	 * Gets the  objects for all the assemblies referenced by this assembly.
	 * @return An array that contains the fully parsed display names of all the
	 * assemblies referenced by this assembly.
	 */
	function GetReferencedAssemblies():cs.NativeArray<cs.system.reflection.AssemblyName>;
	@:overload(function(culture:cs.system.globalization.CultureInfo):cs.system.reflection.Assembly {})
	/**
	 * Gets the satellite assembly for the specified culture.
	 * @param culture The specified culture.
	 * @return The specified satellite assembly.
	 */
	function GetSatelliteAssembly(culture:cs.system.globalization.CultureInfo, version:cs.system.Version):cs.system.reflection.Assembly;
	@:overload(function(name:String):cs.system.Type {})
	@:overload(function(name:String, throwOnError:Bool):cs.system.Type {})
	/**
	 * Gets the  object with the specified name in the assembly instance.
	 * @param name The full name of the type.
	 * @return An object that represents the specified class, or  if the class is not
	 * found.
	 */
	function GetType(name:String, throwOnError:Bool, ignoreCase:Bool):cs.system.Type;
	/**
	 * Gets the types defined in this assembly.
	 * @return An array that contains all the types that are defined in this assembly.
	 */
	function GetTypes():cs.NativeArray<cs.system.Type>;
	/**
	 * Indicates whether or not a specified attribute has been applied to the assembly.
	 * @param attributeType The type of the attribute to be checked for this assembly.
	 * @param inherit This argument is ignored for objects of this type.
	 * @return if the attribute has been applied to the assembly; otherwise, .
	 */
	function IsDefined(attributeType:cs.system.Type, inherit:Bool):Bool;
	@:overload(function(moduleName:String, rawModule:cs.NativeArray<cs.UInt8>):cs.system.reflection.Module {})
	/**
	 * Loads the module, internal to this assembly, with a common object file format
	 * (COFF)-based image containing an emitted module, or a resource file.
	 * @param moduleName The name of the module. This string must correspond to a file
	 * name in this assembly's manifest.
	 * @param rawModule A byte array that is a COFF-based image containing an emitted
	 * module, or a resource.
	 * @return The loaded module.
	 */
	function LoadModule(moduleName:String, rawModule:cs.NativeArray<cs.UInt8>, rawSymbolStore:cs.NativeArray<cs.UInt8>):cs.system.reflection.Module;
	/**
	 * Returns the full name of the assembly, also known as the display name.
	 * @return The full name of the assembly, or the class name if the full name of the
	 * assembly cannot be determined.
	 */
	function ToString():String;
}
