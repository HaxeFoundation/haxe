package cs.system.runtime.compilerservices;

/** Specifies that types that are ordinarily visible only within the current assembly are visible to a specified assembly. */
@:native("System.Runtime.CompilerServices.InternalsVisibleToAttribute")
extern class InternalsVisibleToAttribute extends cs.system.Attribute {
	/**
	 * This property is not implemented.
	 * @return This property does not return a value.
	 */
	var AllInternalsVisible(default, default):Bool;
	/**
	 * Gets the name of the friend assembly to which all types and type members that
	 * are marked with the  keyword are to be made visible.
	 * @return A string that represents the name of the friend assembly.
	 */
	var AssemblyName(default, never):String;
	function new(assemblyName:String):Void;
}
