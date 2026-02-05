package cs.system;

/** Provides a mechanism for retrieving an object to control formatting. */
@:native("System.IFormatProvider")
extern interface IFormatProvider {
	/**
	 * Returns an object that provides formatting services for the specified type.
	 * @param formatType An object that specifies the type of format object to return.
	 * @return An instance of the object specified by , if the  implementation can
	 * supply that type of object; otherwise, .
	 */
	function GetFormat(formatType:cs.system.Type):Dynamic;
}
