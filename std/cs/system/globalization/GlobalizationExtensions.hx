package cs.system.globalization;

/** Provides globalization-related extension methods. */
@:native("System.Globalization.GlobalizationExtensions")
extern class GlobalizationExtensions {
	/**
	 * Returns a  object based on the culture-sensitive string comparison rules of a
	 * specified  object.
	 * @param compareInfo An object that supports culture-sensitive string comparison.
	 * @param options A value that defines how strings should be compared.  is either
	 * the enumeration value , the enumeration value , or a bitwise combination of one
	 * or more of the following values: , , , , , and .
	 * @return An object that can be used to perform string comparisons.
	 */
	static function GetStringComparer(compareInfo:cs.system.globalization.CompareInfo, options:cs.system.globalization.CompareOptions):cs.system.StringComparer;
}
