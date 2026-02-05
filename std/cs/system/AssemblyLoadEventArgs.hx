package cs.system;

/** Provides data for the  event. */
@:native("System.AssemblyLoadEventArgs")
extern class AssemblyLoadEventArgs extends cs.system.EventArgs {
	/**
	 * Gets an  that represents the currently loaded assembly.
	 * @return An instance of  that represents the currently loaded assembly.
	 */
	var LoadedAssembly(default, never):cs.system.reflection.Assembly;
	function new(loadedAssembly:cs.system.reflection.Assembly):Void;
}
