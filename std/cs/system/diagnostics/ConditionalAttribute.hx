package cs.system.diagnostics;

/** Indicates to compilers that a method call or attribute should be ignored unless a specified conditional compilation symbol is defined. */
@:native("System.Diagnostics.ConditionalAttribute")
extern class ConditionalAttribute extends cs.system.Attribute {
	/**
	 * Gets the conditional compilation symbol that is associated with the  attribute.
	 * @return A string that specifies the case-sensitive conditional compilation
	 * symbol that is associated with the  attribute.
	 */
	var ConditionString(default, never):String;
	function new(conditionString:String):Void;
}
