package cs.system.componentmodel.design;

/** Provides an interface to retrieve an assembly or type by name. */
@:native("System.ComponentModel.Design.ITypeResolutionService")
extern interface ITypeResolutionService {
	@:overload(function(name:cs.system.reflection.AssemblyName):cs.system.reflection.Assembly {})
	/**
	 * Gets the requested assembly.
	 * @param name The name of the assembly to retrieve.
	 * @return An instance of the requested assembly, or  if no assembly can be
	 * located.
	 */
	function GetAssembly(name:cs.system.reflection.AssemblyName, throwOnError:Bool):cs.system.reflection.Assembly;
	/**
	 * Gets the path to the file from which the assembly was loaded.
	 * @param name The name of the assembly.
	 * @return The path to the file from which the assembly was loaded.
	 */
	function GetPathOfAssembly(name:cs.system.reflection.AssemblyName):String;
	@:overload(function(name:String):cs.system.Type {})
	@:overload(function(name:String, throwOnError:Bool):cs.system.Type {})
	/**
	 * Loads a type with the specified name.
	 * @param name The name of the type. If the type name is not a fully qualified name
	 * that indicates an assembly, this service will search its internal set of
	 * referenced assemblies.
	 * @return An instance of  that corresponds to the specified name, or  if no type
	 * can be found.
	 */
	function GetType(name:String, throwOnError:Bool, ignoreCase:Bool):cs.system.Type;
	/**
	 * Adds a reference to the specified assembly.
	 * @param name An  that indicates the assembly to reference.
	 */
	function ReferenceAssembly(name:cs.system.reflection.AssemblyName):Void;
}
