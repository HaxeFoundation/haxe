package cs.system.reflection;

/** Specifies the name of a file containing the key pair used to generate a strong name. */
@:native("System.Reflection.AssemblyKeyFileAttribute")
extern class AssemblyKeyFileAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the file containing the key pair used to generate a strong name
	 * for the attributed assembly.
	 * @return A string containing the name of the file that contains the key pair.
	 */
	var KeyFile(default, never):String;
	function new(keyFile:String):Void;
}
