package cs.system.reflection;

/** Defines a trademark custom attribute for an assembly manifest. */
@:native("System.Reflection.AssemblyTrademarkAttribute")
extern class AssemblyTrademarkAttribute extends cs.system.Attribute {
	/**
	 * Gets trademark information.
	 * @return A  containing trademark information.
	 */
	var Trademark(default, never):String;
	function new(trademark:String):Void;
}
