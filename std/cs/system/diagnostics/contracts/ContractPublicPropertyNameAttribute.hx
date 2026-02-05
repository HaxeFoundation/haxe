package cs.system.diagnostics.contracts;

/** Specifies that a field can be used in method contracts when the field has less visibility than the method. */
@:native("System.Diagnostics.Contracts.ContractPublicPropertyNameAttribute")
extern class ContractPublicPropertyNameAttribute extends cs.system.Attribute {
	/**
	 * Gets the property name to be applied to the field.
	 * @return The property name to be applied to the field.
	 */
	var Name(default, never):String;
	function new(name:String):Void;
}
