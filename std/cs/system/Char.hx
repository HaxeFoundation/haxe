package cs.system;

/** Represents a character as a UTF-16 code unit. */
@:native("System.Char")
extern class Char extends cs.system.ValueType {
	/** Represents the largest possible value of a . This field is constant. */
	static var MaxValue(default, never):cs.Char16;
	/** Represents the smallest possible value of a . This field is constant. */
	static var MinValue(default, never):cs.Char16;
	/**
	 * Converts the specified Unicode code point into a UTF-16 encoded string.
	 * @param utf32 A 21-bit Unicode code point.
	 * @return A string consisting of one  object or a surrogate pair of  objects
	 * equivalent to the code point specified by the  parameter.
	 */
	static function ConvertFromUtf32(utf32:Int):String;
	@:overload(function(highSurrogate:cs.Char16, lowSurrogate:cs.Char16):Int {})
	/**
	 * Converts the value of a UTF-16 encoded surrogate pair into a Unicode code point.
	 * @param highSurrogate A high surrogate code unit (that is, a code unit ranging
	 * from U+D800 through U+DBFF).
	 * @param lowSurrogate A low surrogate code unit (that is, a code unit ranging from
	 * U+DC00 through U+DFFF).
	 * @return The 21-bit Unicode code point represented by the  and  parameters.
	 */
	static function ConvertToUtf32(s:String, index:Int):Int;
	@:overload(function(c:cs.Char16):Float {})
	/**
	 * Converts the specified numeric Unicode character to a double-precision floating
	 * point number.
	 * @param c The Unicode character to convert.
	 * @return The numeric value of  if that character represents a number; otherwise,
	 * -1.0.
	 */
	static function GetNumericValue(s:String, index:Int):Float;
	@:overload(function(c:cs.Char16):cs.system.globalization.UnicodeCategory {})
	/**
	 * Categorizes a specified Unicode character into a group identified by one of the 
	 * values.
	 * @param c The Unicode character to categorize.
	 * @return A  value that identifies the group that contains .
	 */
	static function GetUnicodeCategory(s:String, index:Int):cs.system.globalization.UnicodeCategory;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a control
	 * character.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a control character; otherwise, .
	 */
	static function IsControl(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a decimal
	 * digit.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a decimal digit; otherwise, .
	 */
	static function IsDigit(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified  object is a high surrogate.
	 * @param c The Unicode character to evaluate.
	 * @return if the numeric value of the  parameter ranges from U+D800 through
	 * U+DBFF; otherwise, .
	 */
	static function IsHighSurrogate(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a Unicode
	 * letter.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a letter; otherwise, .
	 */
	static function IsLetter(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a letter or
	 * a decimal digit.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a letter or a decimal digit; otherwise, .
	 */
	static function IsLetterOrDigit(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a lowercase
	 * letter.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a lowercase letter; otherwise, .
	 */
	static function IsLower(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified  object is a low surrogate.
	 * @param c The character to evaluate.
	 * @return if the numeric value of the  parameter ranges from U+DC00 through
	 * U+DFFF; otherwise, .
	 */
	static function IsLowSurrogate(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a number.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a number; otherwise, .
	 */
	static function IsNumber(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a
	 * punctuation mark.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a punctuation mark; otherwise, .
	 */
	static function IsPunctuation(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a separator
	 * character.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a separator character; otherwise, .
	 */
	static function IsSeparator(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified character has a surrogate code unit.
	 * @param c The Unicode character to evaluate.
	 * @return if  is either a high surrogate or a low surrogate; otherwise, .
	 */
	static function IsSurrogate(s:String, index:Int):Bool;
	@:overload(function(highSurrogate:cs.Char16, lowSurrogate:cs.Char16):Bool {})
	/**
	 * Indicates whether the two specified  objects form a surrogate pair.
	 * @param highSurrogate The character to evaluate as the high surrogate of a
	 * surrogate pair.
	 * @param lowSurrogate The character to evaluate as the low surrogate of a
	 * surrogate pair.
	 * @return if the numeric value of the  parameter ranges from U+D800 through
	 * U+DBFF, and the numeric value of the  parameter ranges from U+DC00 through
	 * U+DFFF; otherwise, .
	 */
	static function IsSurrogatePair(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as a symbol
	 * character.
	 * @param c The Unicode character to evaluate.
	 * @return if  is a symbol character; otherwise, .
	 */
	static function IsSymbol(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as an uppercase
	 * letter.
	 * @param c The Unicode character to evaluate.
	 * @return if  is an uppercase letter; otherwise, .
	 */
	static function IsUpper(s:String, index:Int):Bool;
	@:overload(function(c:cs.Char16):Bool {})
	/**
	 * Indicates whether the specified Unicode character is categorized as white space.
	 * @param c The Unicode character to evaluate.
	 * @return if  is white space; otherwise, .
	 */
	static function IsWhiteSpace(s:String, index:Int):Bool;
	/**
	 * Converts the value of the specified string to its equivalent Unicode character.
	 * @param s A string that contains a single character, or .
	 * @return A Unicode character equivalent to the sole character in .
	 */
	static function Parse(s:String):cs.Char16;
	@:overload(function(c:cs.Char16):cs.Char16 {})
	/**
	 * Converts the value of a Unicode character to its lowercase equivalent.
	 * @param c The Unicode character to convert.
	 * @return The lowercase equivalent of , or the unchanged value of , if  is already
	 * lowercase or not alphabetic.
	 */
	static function ToLower(c:cs.Char16, culture:cs.system.globalization.CultureInfo):cs.Char16;
	/**
	 * Converts the value of a Unicode character to its lowercase equivalent using the
	 * casing rules of the invariant culture.
	 * @param c The Unicode character to convert.
	 * @return The lowercase equivalent of the  parameter, or the unchanged value of ,
	 * if  is already lowercase or not alphabetic.
	 */
	static function ToLowerInvariant(c:cs.Char16):cs.Char16;
	/**
	 * Converts the value of this instance to its equivalent string representation.
	 * @return The string representation of the value of this instance.
	 */
	static function ToString(c:cs.Char16):String;
	@:overload(function(c:cs.Char16):cs.Char16 {})
	/**
	 * Converts the value of a Unicode character to its uppercase equivalent.
	 * @param c The Unicode character to convert.
	 * @return The uppercase equivalent of , or the unchanged value of  if  is already
	 * uppercase, has no uppercase equivalent, or is not alphabetic.
	 */
	static function ToUpper(c:cs.Char16, culture:cs.system.globalization.CultureInfo):cs.Char16;
	/**
	 * Converts the value of a Unicode character to its uppercase equivalent using the
	 * casing rules of the invariant culture.
	 * @param c The Unicode character to convert.
	 * @return The uppercase equivalent of the  parameter, or the unchanged value of ,
	 * if  is already uppercase or not alphabetic.
	 */
	static function ToUpperInvariant(c:cs.Char16):cs.Char16;
	/**
	 * Converts the value of the specified string to its equivalent Unicode character.
	 * A return code indicates whether the conversion succeeded or failed.
	 * @param s A string that contains a single character, or .
	 * @param result When this method returns, contains a Unicode character equivalent
	 * to the sole character in , if the conversion succeeded, or an undefined value if
	 * the conversion failed. The conversion fails if the  parameter is  or the length
	 * of  is not 1. This parameter is passed uninitialized.
	 * @return if the  parameter was converted successfully; otherwise, .
	 */
	static function TryParse(s:String, result:cs.Ref<cs.Char16>):Bool;
	@:overload(function(value:cs.Char16):Int {})
	/**
	 * Compares this instance to a specified  object and indicates whether this
	 * instance precedes, follows, or appears in the same position in the sort order as
	 * the specified  object.
	 * @param value A  object to compare.
	 * @return A signed number indicating the position of this instance in the sort
	 * order in relation to the  parameter. Return Value Description Less than zero
	 * This instance precedes . Zero This instance has the same position in the sort
	 * order as . Greater than zero This instance follows .
	 */
	function CompareTo(value:Dynamic):Int;
	@:overload(function(obj:cs.Char16):Bool {})
	/**
	 * Returns a value that indicates whether this instance is equal to the specified 
	 * object.
	 * @param obj An object to compare to this instance.
	 * @return if the  parameter equals the value of this instance; otherwise, .
	 */
	function Equals(obj:Dynamic):Bool;
	/**
	 * Returns the hash code for this instance.
	 * @return A 32-bit signed integer hash code.
	 */
	function GetHashCode():Int;
	/**
	 * Returns the  for value type .
	 * @return The enumerated constant, .
	 */
	function GetTypeCode():cs.system.TypeCode;
	@:overload(function():String {})
	/**
	 * Converts the value of this instance to its equivalent string representation.
	 * @return The string representation of the value of this instance.
	 */
	function ToString(provider:cs.system.IFormatProvider):String;
}
