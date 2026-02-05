package cs.system;

/** Provides functionality to format the value of an object into a string representation. */
@:native("System.IFormattable")
extern interface IFormattable {
	/**
	 * Formats the value of the current instance using the specified format.
	 * @param format The format to use. -or- A null reference ( in Visual Basic) to use
	 * the default format defined for the type of the  implementation.
	 * @param formatProvider The provider to use to format the value. -or- A null
	 * reference ( in Visual Basic) to obtain the numeric format information from the
	 * current locale setting of the operating system.
	 * @return The value of the current instance in the specified format.
	 */
	function ToString(format:String, formatProvider:cs.system.IFormatProvider):String;
}
