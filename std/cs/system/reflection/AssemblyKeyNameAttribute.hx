package cs.system.reflection;

/** Specifies the name of a key container within the CSP containing the key pair used to generate a strong name. */
@:native("System.Reflection.AssemblyKeyNameAttribute")
extern class AssemblyKeyNameAttribute extends cs.system.Attribute {
	/**
	 * Gets the name of the container having the key pair that is used to generate a
	 * strong name for the attributed assembly.
	 * @return A string containing the name of the container that has the relevant key
	 * pair.
	 */
	var KeyName(default, never):String;
	function new(keyName:String):Void;
}
