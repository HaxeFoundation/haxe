package cs.system.globalization;

/** Provides information about a specific culture (called a locale for unmanaged code development). The information includes the names for the culture, the writing system, the calendar used, the sort order of strings, and formatting for dates and numbers. */
@:native("System.Globalization.CultureInfo")
extern class CultureInfo {
	/**
	 * Gets or sets the  object that represents the culture used by the current thread.
	 * @return An object that represents the culture used by the current thread.
	 */
	static var CurrentCulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the  object that represents the current user interface culture used
	 * by the Resource Manager to look up culture-specific resources at run time.
	 * @return The culture used by the Resource Manager to look up culture-specific
	 * resources at run time.
	 */
	static var CurrentUICulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the default culture for threads in the current application domain.
	 * @return The default culture for threads in the current application domain, or 
	 * if the current system culture is the default thread culture in the application
	 * domain.
	 */
	static var DefaultThreadCurrentCulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets or sets the default UI culture for threads in the current application
	 * domain.
	 * @return The default UI culture for threads in the current application domain, or
	 * if the current system UI culture is the default thread UI culture in the
	 * application domain.
	 */
	static var DefaultThreadCurrentUICulture(default, default):cs.system.globalization.CultureInfo;
	/**
	 * Gets the  that represents the culture installed with the operating system.
	 * @return The  that represents the culture installed with the operating system.
	 */
	static var InstalledUICulture(default, never):cs.system.globalization.CultureInfo;
	/**
	 * Gets the  object that is culture-independent (invariant).
	 * @return The object that is culture-independent (invariant).
	 */
	static var InvariantCulture(default, never):cs.system.globalization.CultureInfo;
	/**
	 * Gets the default calendar used by the culture.
	 * @return A  that represents the default calendar used by the culture.
	 */
	var Calendar(default, never):cs.system.globalization.Calendar;
	/**
	 * Gets the  that defines how to compare strings for the culture.
	 * @return The  that defines how to compare strings for the culture.
	 */
	var CompareInfo(default, never):cs.system.globalization.CompareInfo;
	/**
	 * Gets the culture types that pertain to the current  object.
	 * @return A bitwise combination of one or more  values. There is no default value.
	 */
	var CultureTypes(default, never):cs.system.globalization.CultureTypes;
	/**
	 * Gets or sets a  that defines the culturally appropriate format of displaying
	 * dates and times.
	 * @return A  that defines the culturally appropriate format of displaying dates
	 * and times.
	 */
	var DateTimeFormat(default, default):cs.system.globalization.DateTimeFormatInfo;
	/**
	 * Gets the full localized culture name.
	 * @return The full localized culture name in the format languagefull
	 * [country/regionfull], where languagefull is the full name of the language and
	 * country/regionfull is the full name of the country/region.
	 */
	var DisplayName(default, never):String;
	/**
	 * Gets the culture name in the format languagefull [country/regionfull] in
	 * English.
	 * @return The culture name in the format languagefull [country/regionfull] in
	 * English, where languagefull is the full name of the language and
	 * country/regionfull is the full name of the country/region.
	 */
	var EnglishName(default, never):String;
	/**
	 * Deprecated. Gets the RFC 4646 standard identification for a language.
	 * @return A string that is the RFC 4646 standard identification for a language.
	 */
	var IetfLanguageTag(default, never):String;
	/**
	 * Gets a value indicating whether the current  represents a neutral culture.
	 * @return if the current  represents a neutral culture; otherwise, .
	 */
	var IsNeutralCulture(default, never):Bool;
	/**
	 * Gets a value indicating whether the current  is read-only.
	 * @return if the current  is read-only; otherwise, . The default is .
	 */
	var IsReadOnly(default, never):Bool;
	/**
	 * Gets the active input locale identifier.
	 * @return A 32-bit signed number that specifies an input locale identifier.
	 */
	var KeyboardLayoutId(default, never):Int;
	/**
	 * Gets the culture identifier for the current .
	 * @return The culture identifier for the current .
	 */
	var LCID(default, never):Int;
	/**
	 * Gets the culture name in the format languagecode2-country/regioncode2.
	 * @return The culture name in the format languagecode2-country/regioncode2.
	 * languagecode2 is a lowercase two-letter code derived from ISO 639-1.
	 * country/regioncode2 is derived from ISO 3166 and usually consists of two
	 * uppercase letters, or a BCP-47 language tag.
	 */
	var Name(default, never):String;
	/**
	 * Gets the culture name, consisting of the language, the country/region, and the
	 * optional script, that the culture is set to display.
	 * @return The culture name. consisting of the full name of the language, the full
	 * name of the country/region, and the optional script. The format is discussed in
	 * the description of the  class.
	 */
	var NativeName(default, never):String;
	/**
	 * Gets or sets a  that defines the culturally appropriate format of displaying
	 * numbers, currency, and percentage.
	 * @return A  that defines the culturally appropriate format of displaying numbers,
	 * currency, and percentage.
	 */
	var NumberFormat(default, default):cs.system.globalization.NumberFormatInfo;
	/**
	 * Gets the list of calendars that can be used by the culture.
	 * @return An array of type  that represents the calendars that can be used by the
	 * culture represented by the current .
	 */
	var OptionalCalendars(default, never):cs.NativeArray<cs.system.globalization.Calendar>;
	/**
	 * Gets the  that represents the parent culture of the current .
	 * @return The  that represents the parent culture of the current .
	 */
	var Parent(default, never):cs.system.globalization.CultureInfo;
	/**
	 * Gets the  that defines the writing system associated with the culture.
	 * @return The  that defines the writing system associated with the culture.
	 */
	var TextInfo(default, never):cs.system.globalization.TextInfo;
	/**
	 * Gets the ISO 639-2 three-letter code for the language of the current .
	 * @return The ISO 639-2 three-letter code for the language of the current .
	 */
	var ThreeLetterISOLanguageName(default, never):String;
	/**
	 * Gets the three-letter code for the language as defined in the Windows API.
	 * @return The three-letter code for the language as defined in the Windows API.
	 */
	var ThreeLetterWindowsLanguageName(default, never):String;
	/**
	 * Gets the ISO 639-1 two-letter code for the language of the current .
	 * @return The ISO 639-1 two-letter code for the language of the current .
	 */
	var TwoLetterISOLanguageName(default, never):String;
	/**
	 * Gets a value indicating whether the current  object uses the user-selected
	 * culture settings.
	 * @return if the current  uses the user-selected culture settings; otherwise, .
	 */
	var UseUserOverride(default, never):Bool;
	@:overload(function(culture:Int):Void {})
	@:overload(function(name:String):Void {})
	@:overload(function(culture:Int, useUserOverride:Bool):Void {})
	function new(name:String, useUserOverride:Bool):Void;
	/**
	 * Creates a  that represents the specific culture that is associated with the
	 * specified name.
	 * @param name A predefined  name or the name of an existing  object.  is not
	 * case-sensitive.
	 * @return A  object that represents: The invariant culture, if  is an empty string
	 * (""). -or- The specific culture associated with , if  is a neutral culture. -or-
	 * The culture specified by , if  is already a specific culture.
	 */
	static function CreateSpecificCulture(name:String):cs.system.globalization.CultureInfo;
	@:overload(function(culture:Int):cs.system.globalization.CultureInfo {})
	@:overload(function(name:String):cs.system.globalization.CultureInfo {})
	/**
	 * Retrieves a cached, read-only instance of a culture by using the specified
	 * culture identifier.
	 * @param culture A locale identifier (LCID).
	 * @return A read-only  object.
	 */
	static function GetCultureInfo(name:String, altName:String):cs.system.globalization.CultureInfo;
	/**
	 * Deprecated. Retrieves a read-only  object having linguistic characteristics that
	 * are identified by the specified RFC 4646 language tag.
	 * @param name The name of a language as specified by the RFC 4646 standard.
	 * @return A read-only  object.
	 */
	static function GetCultureInfoByIetfLanguageTag(name:String):cs.system.globalization.CultureInfo;
	/**
	 * Gets the list of supported cultures filtered by the specified  parameter.
	 * @param types A bitwise combination of the enumeration values that filter the
	 * cultures to retrieve.
	 * @return An array that contains the cultures specified by the  parameter. The
	 * array of cultures is unsorted.
	 */
	static function GetCultures(types:cs.system.globalization.CultureTypes):cs.NativeArray<cs.system.globalization.CultureInfo>;
	/**
	 * Returns a read-only wrapper around the specified  object.
	 * @param ci The  object to wrap.
	 * @return A read-only  wrapper around .
	 */
	static function ReadOnly(ci:cs.system.globalization.CultureInfo):cs.system.globalization.CultureInfo;
	/** Refreshes cached culture-related information. */
	function ClearCachedData():Void;
	/**
	 * Creates a copy of the current .
	 * @return A copy of the current .
	 */
	function Clone():Dynamic;
	/**
	 * Determines whether the specified object is the same culture as the current .
	 * @param value The object to compare with the current .
	 * @return if  is the same culture as the current ; otherwise, .
	 */
	function Equals(value:Dynamic):Bool;
	/**
	 * Gets an alternate user interface culture suitable for console applications when
	 * the default graphic user interface culture is unsuitable.
	 * @return An alternate culture that is used to read and display text on the
	 * console.
	 */
	function GetConsoleFallbackUICulture():cs.system.globalization.CultureInfo;
	/**
	 * Gets an object that defines how to format the specified type.
	 * @param formatType The  for which to get a formatting object. This method only
	 * supports the  and  types.
	 * @return The value of the  property, which is a  containing the default number
	 * format information for the current , if  is the  object for the  class. -or- The
	 * value of the  property, which is a  containing the default date and time format
	 * information for the current , if  is the  object for the  class. -or- null, if 
	 * is any other object.
	 */
	function GetFormat(formatType:cs.system.Type):Dynamic;
	/**
	 * Serves as a hash function for the current , suitable for hashing algorithms and
	 * data structures, such as a hash table.
	 * @return A hash code for the current .
	 */
	function GetHashCode():Int;
	/**
	 * Returns a string containing the name of the current  in the format
	 * languagecode2-country/regioncode2.
	 * @return A string containing the name of the current .
	 */
	function ToString():String;
}
