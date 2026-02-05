package cs.system.globalization;

/** Enumerates the text elements of a string. */
@:native("System.Globalization.TextElementEnumerator")
extern class TextElementEnumerator {
	/**
	 * Gets the current text element in the string.
	 * @return An object containing the current text element in the string.
	 */
	var Current(default, never):Dynamic;
	/**
	 * Gets the index of the text element that the enumerator is currently positioned
	 * over.
	 * @return The index of the text element that the enumerator is currently
	 * positioned over.
	 */
	var ElementIndex(default, never):Int;
	/**
	 * Gets the current text element in the string.
	 * @return A new string containing the current text element in the string being
	 * read.
	 */
	function GetTextElement():String;
	/**
	 * Advances the enumerator to the next text element of the string.
	 * @return if the enumerator was successfully advanced to the next text element; 
	 * if the enumerator has passed the end of the string.
	 */
	function MoveNext():Bool;
	/** Sets the enumerator to its initial position, which is before the first text element in the string. */
	function Reset():Void;
}
