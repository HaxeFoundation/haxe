package cs.system.globalization;

/** Retrieves information about a Unicode character. This class cannot be inherited. */
@:native("System.Globalization.CharUnicodeInfo")
extern class CharUnicodeInfo {
	@:overload(function(ch:cs.Char16):Int {})
	/**
	 * Gets the decimal digit value of the specified numeric character.
	 * @param ch The Unicode character for which to get the decimal digit value.
	 * @return The decimal digit value of the specified numeric character. -or- -1, if
	 * the specified character is not a decimal digit.
	 */
	static function GetDecimalDigitValue(s:String, index:Int):Int;
	@:overload(function(ch:cs.Char16):Int {})
	/**
	 * Gets the digit value of the specified numeric character.
	 * @param ch The Unicode character for which to get the digit value.
	 * @return The digit value of the specified numeric character. -or- -1, if the
	 * specified character is not a digit.
	 */
	static function GetDigitValue(s:String, index:Int):Int;
	@:overload(function(ch:cs.Char16):Float {})
	/**
	 * Gets the numeric value associated with the specified character.
	 * @param ch The Unicode character for which to get the numeric value.
	 * @return The numeric value associated with the specified character. -or- -1, if
	 * the specified character is not a numeric character.
	 */
	static function GetNumericValue(s:String, index:Int):Float;
	@:overload(function(ch:cs.Char16):cs.system.globalization.UnicodeCategory {})
	@:overload(function(codePoint:Int):cs.system.globalization.UnicodeCategory {})
	/**
	 * Gets the Unicode category of the specified character.
	 * @param ch The Unicode character for which to get the Unicode category.
	 * @return A  value indicating the category of the specified character.
	 */
	static function GetUnicodeCategory(s:String, index:Int):cs.system.globalization.UnicodeCategory;
}
