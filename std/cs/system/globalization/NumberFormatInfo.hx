package cs.system.globalization;

/** Provides culture-specific information for formatting and parsing numeric values. */
@:native("System.Globalization.NumberFormatInfo")
extern class NumberFormatInfo {
	/**
	 * Gets a read-only  that formats values based on the current culture.
	 * @return A read-only  based on the culture of the current thread.
	 */
	static var CurrentInfo(default, never):cs.system.globalization.NumberFormatInfo;
	/**
	 * Gets a read-only  object that is culture-independent (invariant).
	 * @return A read-only  object that is culture-independent (invariant).
	 */
	static var InvariantInfo(default, never):cs.system.globalization.NumberFormatInfo;
	/**
	 * Gets or sets the number of decimal places to use in currency values.
	 * @return The number of decimal places to use in currency values. The default for 
	 * is 2.
	 */
	var CurrencyDecimalDigits(default, default):Int;
	/**
	 * Gets or sets the string to use as the decimal separator in currency values.
	 * @return The string to use as the decimal separator in currency values. The
	 * default for  is ".".
	 */
	var CurrencyDecimalSeparator(default, default):String;
	/**
	 * Gets or sets the string that separates groups of digits to the left of the
	 * decimal in currency values.
	 * @return The string that separates groups of digits to the left of the decimal in
	 * currency values. The default for  is ",".
	 */
	var CurrencyGroupSeparator(default, default):String;
	/**
	 * Gets or sets the number of digits in each group to the left of the decimal in
	 * currency values.
	 * @return The number of digits in each group to the left of the decimal in
	 * currency values. The default for  is a one-dimensional array with only one
	 * element, which is set to 3.
	 */
	var CurrencyGroupSizes(default, default):cs.NativeArray<Int>;
	/**
	 * Gets or sets the format pattern for negative currency values.
	 * @return The format pattern for negative currency values. The default for  is 0,
	 * which represents "($n)", where "$" is the  and  is a number.
	 */
	var CurrencyNegativePattern(default, default):Int;
	/**
	 * Gets or sets the format pattern for positive currency values.
	 * @return The format pattern for positive currency values. The default for  is 0,
	 * which represents "$n", where "$" is the  and  is a number.
	 */
	var CurrencyPositivePattern(default, default):Int;
	/**
	 * Gets or sets the string to use as the currency symbol.
	 * @return The string to use as the currency symbol. The default for  is "¤".
	 */
	var CurrencySymbol(default, default):String;
	/**
	 * Gets or sets a value that specifies how the graphical user interface displays
	 * the shape of a digit.
	 * @return One of the enumeration values that specifies the culture-specific digit
	 * shape.
	 */
	var DigitSubstitution(default, default):cs.system.globalization.DigitShapes;
	/**
	 * Gets a value that indicates whether this  object is read-only.
	 * @return if the  is read-only; otherwise, .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets or sets the string that represents the IEEE NaN (not a number) value.
	 * @return The string that represents the IEEE NaN (not a number) value. The
	 * default for  is "NaN".
	 */
	var NaNSymbol(default, default):String;
	/**
	 * Gets or sets a string array of native digits equivalent to the Western digits 0
	 * through 9.
	 * @return A string array that contains the native equivalent of the Western digits
	 * 0 through 9. The default is an array having the elements "0", "1", "2", "3",
	 * "4", "5", "6", "7", "8", and "9".
	 */
	var NativeDigits(default, default):cs.NativeArray<String>;
	/**
	 * Gets or sets the string that represents negative infinity.
	 * @return The string that represents negative infinity. The default for  is
	 * "-Infinity".
	 */
	var NegativeInfinitySymbol(default, default):String;
	/**
	 * Gets or sets the string that denotes that the associated number is negative.
	 * @return The string that denotes that the associated number is negative. The
	 * default for  is "-".
	 */
	var NegativeSign(default, default):String;
	/**
	 * Gets or sets the number of decimal places to use in numeric values.
	 * @return The number of decimal places to use in numeric values. The default for 
	 * is 2.
	 */
	var NumberDecimalDigits(default, default):Int;
	/**
	 * Gets or sets the string to use as the decimal separator in numeric values.
	 * @return The string to use as the decimal separator in numeric values. The
	 * default for  is ".".
	 */
	var NumberDecimalSeparator(default, default):String;
	/**
	 * Gets or sets the string that separates groups of digits to the left of the
	 * decimal in numeric values.
	 * @return The string that separates groups of digits to the left of the decimal in
	 * numeric values. The default for  is ",".
	 */
	var NumberGroupSeparator(default, default):String;
	/**
	 * Gets or sets the number of digits in each group to the left of the decimal in
	 * numeric values.
	 * @return The number of digits in each group to the left of the decimal in numeric
	 * values. The default for  is a one-dimensional array with only one element, which
	 * is set to 3.
	 */
	var NumberGroupSizes(default, default):cs.NativeArray<Int>;
	/**
	 * Gets or sets the format pattern for negative numeric values.
	 * @return The format pattern for negative numeric values.
	 */
	var NumberNegativePattern(default, default):Int;
	/**
	 * Gets or sets the number of decimal places to use in percent values.
	 * @return The number of decimal places to use in percent values. The default for 
	 * is 2.
	 */
	var PercentDecimalDigits(default, default):Int;
	/**
	 * Gets or sets the string to use as the decimal separator in percent values.
	 * @return The string to use as the decimal separator in percent values. The
	 * default for  is ".".
	 */
	var PercentDecimalSeparator(default, default):String;
	/**
	 * Gets or sets the string that separates groups of digits to the left of the
	 * decimal in percent values.
	 * @return The string that separates groups of digits to the left of the decimal in
	 * percent values. The default for  is ",".
	 */
	var PercentGroupSeparator(default, default):String;
	/**
	 * Gets or sets the number of digits in each group to the left of the decimal in
	 * percent values.
	 * @return The number of digits in each group to the left of the decimal in percent
	 * values. The default for  is a one-dimensional array with only one element, which
	 * is set to 3.
	 */
	var PercentGroupSizes(default, default):cs.NativeArray<Int>;
	/**
	 * Gets or sets the format pattern for negative percent values.
	 * @return The format pattern for negative percent values. The default for  is 0,
	 * which represents "-n %", where "%" is the  and  is a number.
	 */
	var PercentNegativePattern(default, default):Int;
	/**
	 * Gets or sets the format pattern for positive percent values.
	 * @return The format pattern for positive percent values. The default for  is 0,
	 * which represents "n %", where "%" is the  and  is a number.
	 */
	var PercentPositivePattern(default, default):Int;
	/**
	 * Gets or sets the string to use as the percent symbol.
	 * @return The string to use as the percent symbol. The default for  is "%".
	 */
	var PercentSymbol(default, default):String;
	/**
	 * Gets or sets the string to use as the per mille symbol.
	 * @return The string to use as the per mille symbol. The default for  is "‰",
	 * which is the Unicode character U+2030.
	 */
	var PerMilleSymbol(default, default):String;
	/**
	 * Gets or sets the string that represents positive infinity.
	 * @return The string that represents positive infinity. The default for  is
	 * "Infinity".
	 */
	var PositiveInfinitySymbol(default, default):String;
	/**
	 * Gets or sets the string that denotes that the associated number is positive.
	 * @return The string that denotes that the associated number is positive. The
	 * default for  is "+".
	 */
	var PositiveSign(default, default):String;
	function new():Void;
	/**
	 * Gets the  associated with the specified .
	 * @param formatProvider The  used to get the . -or- to get .
	 * @return The  associated with the specified .
	 */
	static function GetInstance(formatProvider:cs.system.IFormatProvider):cs.system.globalization.NumberFormatInfo;
	/**
	 * Returns a read-only  wrapper.
	 * @param nfi The  to wrap.
	 * @return A read-only  wrapper around .
	 */
	static function ReadOnly(nfi:cs.system.globalization.NumberFormatInfo):cs.system.globalization.NumberFormatInfo;
	/**
	 * Creates a shallow copy of the  object.
	 * @return A new object copied from the original  object.
	 */
	function Clone():Dynamic;
	/**
	 * Gets an object of the specified type that provides a number formatting service.
	 * @param formatType The  of the required formatting service.
	 * @return The current , if  is the same as the type of the current ; otherwise, .
	 */
	function GetFormat(formatType:cs.system.Type):Dynamic;
}
