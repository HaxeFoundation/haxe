package cs.system.reflection;

/** Provides a remotable version of the . */
@:native("System.Reflection.AssemblyNameProxy")
extern class AssemblyNameProxy extends cs.system.MarshalByRefObject {
	function new():Void;
	/**
	 * Gets the  for a given file.
	 * @param assemblyFile The assembly file for which to get the .
	 * @return An  object representing the given file.
	 */
	function GetAssemblyName(assemblyFile:String):cs.system.reflection.AssemblyName;
}
