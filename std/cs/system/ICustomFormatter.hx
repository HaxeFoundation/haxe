package cs.system;

/** Defines a method that supports custom formatting of the value of an object. */
@:native("System.ICustomFormatter")
extern interface ICustomFormatter {
	/**
	 * Converts the value of a specified object to an equivalent string representation
	 * using specified format and culture-specific formatting information.
	 * @param format A format string containing formatting specifications.
	 * @param arg An object to format.
	 * @param formatProvider An object that supplies format information about the
	 * current instance.
	 * @return The string representation of the value of , formatted as specified by 
	 * and .
	 */
	function Format(format:String, arg:Dynamic, formatProvider:cs.system.IFormatProvider):String;
}
