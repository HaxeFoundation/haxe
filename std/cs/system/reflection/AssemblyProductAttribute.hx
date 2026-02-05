package cs.system.reflection;

/** Defines a product name custom attribute for an assembly manifest. */
@:native("System.Reflection.AssemblyProductAttribute")
extern class AssemblyProductAttribute extends cs.system.Attribute {
	/**
	 * Gets product name information.
	 * @return A string containing the product name.
	 */
	var Product(default, never):String;
	function new(product:String):Void;
}
