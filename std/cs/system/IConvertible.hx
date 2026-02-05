package cs.system;

/** Defines methods that convert the value of the implementing reference or value type to a common language runtime type that has an equivalent value. */
@:native("System.IConvertible")
extern interface IConvertible {
	/**
	 * Returns the  for this instance.
	 * @return The enumerated constant that is the  of the class or value type that
	 * implements this interface.
	 */
	function GetTypeCode():cs.system.TypeCode;
	/**
	 * Converts the value of this instance to an equivalent Boolean value using the
	 * specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A Boolean value equivalent to the value of this instance.
	 */
	function ToBoolean(provider:cs.system.IFormatProvider):Bool;
	/**
	 * Converts the value of this instance to an equivalent 8-bit unsigned integer
	 * using the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 8-bit unsigned integer equivalent to the value of this instance.
	 */
	function ToByte(provider:cs.system.IFormatProvider):cs.UInt8;
	/**
	 * Converts the value of this instance to an equivalent Unicode character using the
	 * specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A Unicode character equivalent to the value of this instance.
	 */
	function ToChar(provider:cs.system.IFormatProvider):cs.Char16;
	/**
	 * Converts the value of this instance to an equivalent  using the specified
	 * culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A  instance equivalent to the value of this instance.
	 */
	function ToDateTime(provider:cs.system.IFormatProvider):cs.system.DateTime;
	/**
	 * Converts the value of this instance to an equivalent  number using the specified
	 * culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A  number equivalent to the value of this instance.
	 */
	function ToDecimal(provider:cs.system.IFormatProvider):cs.system.Decimal;
	/**
	 * Converts the value of this instance to an equivalent double-precision
	 * floating-point number using the specified culture-specific formatting
	 * information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A double-precision floating-point number equivalent to the value of this
	 * instance.
	 */
	function ToDouble(provider:cs.system.IFormatProvider):Float;
	/**
	 * Converts the value of this instance to an equivalent 16-bit signed integer using
	 * the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 16-bit signed integer equivalent to the value of this instance.
	 */
	function ToInt16(provider:cs.system.IFormatProvider):cs.Int16;
	/**
	 * Converts the value of this instance to an equivalent 32-bit signed integer using
	 * the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 32-bit signed integer equivalent to the value of this instance.
	 */
	function ToInt32(provider:cs.system.IFormatProvider):Int;
	/**
	 * Converts the value of this instance to an equivalent 64-bit signed integer using
	 * the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 64-bit signed integer equivalent to the value of this instance.
	 */
	function ToInt64(provider:cs.system.IFormatProvider):haxe.Int64;
	/**
	 * Converts the value of this instance to an equivalent 8-bit signed integer using
	 * the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 8-bit signed integer equivalent to the value of this instance.
	 */
	function ToSByte(provider:cs.system.IFormatProvider):cs.Int8;
	/**
	 * Converts the value of this instance to an equivalent single-precision
	 * floating-point number using the specified culture-specific formatting
	 * information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A single-precision floating-point number equivalent to the value of this
	 * instance.
	 */
	function ToSingle(provider:cs.system.IFormatProvider):Single;
	/**
	 * Converts the value of this instance to an equivalent  using the specified
	 * culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return A  instance equivalent to the value of this instance.
	 */
	function ToString(provider:cs.system.IFormatProvider):String;
	/**
	 * Converts the value of this instance to an  of the specified  that has an
	 * equivalent value, using the specified culture-specific formatting information.
	 * @param conversionType The  to which the value of this instance is converted.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An  instance of type  whose value is equivalent to the value of this
	 * instance.
	 */
	function ToType(conversionType:cs.system.Type, provider:cs.system.IFormatProvider):Dynamic;
	/**
	 * Converts the value of this instance to an equivalent 16-bit unsigned integer
	 * using the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 16-bit unsigned integer equivalent to the value of this instance.
	 */
	function ToUInt16(provider:cs.system.IFormatProvider):cs.UInt16;
	/**
	 * Converts the value of this instance to an equivalent 32-bit unsigned integer
	 * using the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 32-bit unsigned integer equivalent to the value of this instance.
	 */
	function ToUInt32(provider:cs.system.IFormatProvider):cs.UInt;
	/**
	 * Converts the value of this instance to an equivalent 64-bit unsigned integer
	 * using the specified culture-specific formatting information.
	 * @param provider An  interface implementation that supplies culture-specific
	 * formatting information.
	 * @return An 64-bit unsigned integer equivalent to the value of this instance.
	 */
	function ToUInt64(provider:cs.system.IFormatProvider):cs.UInt64;
}
