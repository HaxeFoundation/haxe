package cs.system.linq.expressions;

/** Used to represent the target of a . */
@:native("System.Linq.Expressions.LabelTarget")
extern class LabelTarget {
	/**
	 * Gets the name of the label.
	 * @return The name of the label.
	 */
	var Name(default, never):String;
	/**
	 * The type of value that is passed when jumping to the label (or  if no value
	 * should be passed).
	 * @return The  object representing the type of the value that is passed when
	 * jumping to the label or  if no value should be passed
	 */
	var Type(default, never):cs.system.Type;
	/**
	 * Returns a  that represents the current .
	 * @return A  that represents the current .
	 */
	function ToString():String;
}
