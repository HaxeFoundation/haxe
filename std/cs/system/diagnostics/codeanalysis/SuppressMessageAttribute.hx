package cs.system.diagnostics.codeanalysis;

/** Suppresses reporting of a specific static analysis tool rule violation, allowing multiple suppressions on a single code artifact. */
@:native("System.Diagnostics.CodeAnalysis.SuppressMessageAttribute")
extern class SuppressMessageAttribute extends cs.system.Attribute {
	/**
	 * Gets the category identifying the classification of the attribute.
	 * @return The category identifying the attribute.
	 */
	var Category(default, never):String;
	/**
	 * Gets the identifier of the static analysis tool rule to be suppressed.
	 * @return The identifier of the static analysis tool rule to be suppressed.
	 */
	var CheckId(default, never):String;
	/**
	 * Gets or sets the justification for suppressing the code analysis message.
	 * @return The justification for suppressing the message.
	 */
	var Justification(default, default):String;
	/**
	 * Gets or sets an optional argument expanding on exclusion criteria.
	 * @return A string containing the expanded exclusion criteria.
	 */
	var MessageId(default, default):String;
	/**
	 * Gets or sets the scope of the code that is relevant for the attribute.
	 * @return The scope of the code that is relevant for the attribute.
	 */
	var Scope(default, default):String;
	/**
	 * Gets or sets a fully qualified path that represents the target of the attribute.
	 * @return A fully qualified path that represents the target of the attribute.
	 */
	var Target(default, default):String;
	function new(category:String, checkId:String):Void;
}
