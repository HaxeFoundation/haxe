package cs.system;

/** Provides extension methods to work with string normalization. */
@:native("System.StringNormalizationExtensions")
extern class StringNormalizationExtensions {
	@:overload(function(value:String):Bool {})
	/**
	 * Indicates whether the specified string is in Unicode normalization form C.
	 * @param value A string.
	 * @return if  is in normalization form C; otherwise, .
	 */
	static function IsNormalized(value:String, normalizationForm:cs.system.text.NormalizationForm):Bool;
	@:overload(function(value:String):String {})
	/**
	 * Normalizes a string to a Unicode normalization form C.
	 * @param value The string to normalize.
	 * @return A new string whose textual value is the same as  but whose binary
	 * representation is in Unicode normalization form C.
	 */
	static function Normalize(value:String, normalizationForm:cs.system.text.NormalizationForm):String;
}
