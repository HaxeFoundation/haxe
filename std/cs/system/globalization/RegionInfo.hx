package cs.system.globalization;

/** Contains information about the country/region. */
@:native("System.Globalization.RegionInfo")
extern class RegionInfo {
	/**
	 * Gets the  that represents the country/region used by the current thread.
	 * @return The  that represents the country/region used by the current thread.
	 */
	static var CurrentRegion(default, never):cs.system.globalization.RegionInfo;
	/**
	 * Gets the name, in English, of the currency used in the country/region.
	 * @return The name, in English, of the currency used in the country/region.
	 */
	var CurrencyEnglishName(default, never):String;
	/**
	 * Gets the name of the currency used in the country/region, formatted in the
	 * native language of the country/region.
	 * @return The native name of the currency used in the country/region, formatted in
	 * the language associated with the ISO 3166 country/region code.
	 */
	var CurrencyNativeName(default, never):String;
	/**
	 * Gets the currency symbol associated with the country/region.
	 * @return The currency symbol associated with the country/region.
	 */
	var CurrencySymbol(default, never):String;
	/**
	 * Gets the full name of the country/region in the language of the localized
	 * version of .NET Framework.
	 * @return The full name of the country/region in the language of the localized
	 * version of .NET Framework.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets the full name of the country/region in English.
	 * @return The full name of the country/region in English.
	 */
	var EnglishName(default, never):String;
	/**
	 * Gets a unique identification number for a geographical region, country, city, or
	 * location.
	 * @return A 32-bit signed number that uniquely identifies a geographical location.
	 */
	var GeoId(default, never):Int;
	/**
	 * Gets a value indicating whether the country/region uses the metric system for
	 * measurements.
	 * @return if the country/region uses the metric system for measurements;
	 * otherwise, .
	 */
	var IsMetric(default, never):Bool;
	/**
	 * Gets the three-character ISO 4217 currency symbol associated with the
	 * country/region.
	 * @return The three-character ISO 4217 currency symbol associated with the
	 * country/region.
	 */
	var ISOCurrencySymbol(default, never):String;
	/**
	 * Gets the name or ISO 3166 two-letter country/region code for the current 
	 * object.
	 * @return The value specified by the  parameter of the  constructor. The return
	 * value is in uppercase. -or- The two-letter code defined in ISO 3166 for the
	 * country/region specified by the  parameter of the  constructor. The return value
	 * is in uppercase.
	 */
	var Name(default, never):String;
	/**
	 * Gets the name of a country/region formatted in the native language of the
	 * country/region.
	 * @return The native name of the country/region formatted in the language
	 * associated with the ISO 3166 country/region code.
	 */
	var NativeName(default, never):String;
	/**
	 * Gets the three-letter code defined in ISO 3166 for the country/region.
	 * @return The three-letter code defined in ISO 3166 for the country/region.
	 */
	var ThreeLetterISORegionName(default, never):String;
	/**
	 * Gets the three-letter code assigned by Windows to the country/region represented
	 * by this .
	 * @return The three-letter code assigned by Windows to the country/region
	 * represented by this .
	 */
	var ThreeLetterWindowsRegionName(default, never):String;
	/**
	 * Gets the two-letter code defined in ISO 3166 for the country/region.
	 * @return The two-letter code defined in ISO 3166 for the country/region.
	 */
	var TwoLetterISORegionName(default, never):String;
	@:overload(function(culture:Int):Void {})
	function new(name:String):Void;
	/**
	 * Determines whether the specified object is the same instance as the current .
	 * @param value The object to compare with the current .
	 * @return if the  parameter is a  object and its  property is the same as the 
	 * property of the current  object; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Serves as a hash function for the current , suitable for hashing algorithms and
	 * data structures, such as a hash table.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string containing the culture name or ISO 3166 two-letter
	 * country/region codes specified for the current .
	 * @return A string containing the culture name or ISO 3166 two-letter
	 * country/region codes defined for the current .
	 */
	function ToString():String;
}
