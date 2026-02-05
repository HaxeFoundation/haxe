package cs.system;

/** Provides an object representation of a uniform resource identifier (URI) and easy access to the parts of the URI. */
@:native("System.Uri")
extern class Uri {
	/** Specifies the characters that separate the communication protocol scheme from the address portion of the URI. This field is read-only. */
	static var SchemeDelimiter(default, never):String;
	/** Specifies that the URI is a pointer to a file. This field is read-only. */
	static var UriSchemeFile(default, never):String;
	/** Specifies that the URI is accessed through the File Transfer Protocol (FTP). This field is read-only. */
	static var UriSchemeFtp(default, never):String;
	/** Specifies that the URI is accessed through the Gopher protocol. This field is read-only. */
	static var UriSchemeGopher(default, never):String;
	/** Specifies that the URI is accessed through the Hypertext Transfer Protocol (HTTP). This field is read-only. */
	static var UriSchemeHttp(default, never):String;
	/** Specifies that the URI is accessed through the Secure Hypertext Transfer Protocol (HTTPS). This field is read-only. */
	static var UriSchemeHttps(default, never):String;
	/** Specifies that the URI is an email address and is accessed through the Simple Mail Transport Protocol (SMTP). This field is read-only. */
	static var UriSchemeMailto(default, never):String;
	/** Specifies that the URI is accessed through the NetPipe scheme used by Windows Communication Foundation (WCF). This field is read-only. */
	static var UriSchemeNetPipe(default, never):String;
	/** Specifies that the URI is accessed through the NetTcp scheme used by Windows Communication Foundation (WCF). This field is read-only. */
	static var UriSchemeNetTcp(default, never):String;
	/** Specifies that the URI is an Internet news group and is accessed through the Network News Transport Protocol (NNTP). This field is read-only. */
	static var UriSchemeNews(default, never):String;
	/** Specifies that the URI is an Internet news group and is accessed through the Network News Transport Protocol (NNTP). This field is read-only. */
	static var UriSchemeNntp(default, never):String;
	/**
	 * Gets the absolute path of the URI.
	 * @return A  containing the absolute path to the resource.
	 */
	var AbsolutePath(default, never):String;
	/**
	 * Gets the absolute URI.
	 * @return A  containing the entire URI.
	 */
	var AbsoluteUri(default, never):String;
	/**
	 * Gets the Domain Name System (DNS) host name or IP address and the port number
	 * for a server.
	 * @return A  containing the authority component of the URI represented by this
	 * instance.
	 */
	var Authority(default, never):String;
	/**
	 * Gets a host name that, after being unescaped if necessary, is safe to use for
	 * DNS resolution.
	 * @return A  that contains the host part of the URI in a format suitable for DNS
	 * resolution; or the original host string, if it is already suitable for
	 * resolution.
	 */
	var DnsSafeHost(default, never):String;
	/**
	 * Gets the escaped URI fragment.
	 * @return A  that contains any URI fragment information.
	 */
	var Fragment(default, never):String;
	/**
	 * Gets the host component of this instance.
	 * @return A  that contains the host name. This is usually the DNS host name or IP
	 * address of the server.
	 */
	var Host(default, never):String;
	/**
	 * Gets the type of the host name specified in the URI.
	 * @return A member of the  enumeration.
	 */
	var HostNameType(default, never):cs.system.UriHostNameType;
	/**
	 * The RFC 3490 compliant International Domain Name of the host, using Punycode as
	 * appropriate. This string, after being unescaped if necessary, is safe to use for
	 * DNS resolution.
	 * @return The hostname, formatted with Punycode according to the IDN standard.
	 */
	var IdnHost(default, never):String;
	/**
	 * Gets whether the  instance is absolute.
	 * @return A  value that is  if the  instance is absolute; otherwise, .
	 */
	var IsAbsoluteUri(default, never):Bool;
	/**
	 * Gets whether the port value of the URI is the default for this scheme.
	 * @return A  value that is  if the value in the  property is the default port for
	 * this scheme; otherwise, .
	 */
	var IsDefaultPort(default, never):Bool;
	/**
	 * Gets a value indicating whether the specified  is a file URI.
	 * @return A  value that is  if the  is a file URI; otherwise, .
	 */
	var IsFile(default, never):Bool;
	/**
	 * Gets whether the specified  references the local host.
	 * @return A  value that is  if this  references the local host; otherwise, .
	 */
	var IsLoopback(default, never):Bool;
	/**
	 * Gets whether the specified  is a universal naming convention (UNC) path.
	 * @return A  value that is  if the  is a UNC path; otherwise, .
	 */
	var IsUnc(default, never):Bool;
	/**
	 * Gets a local operating-system representation of a file name.
	 * @return A  that contains the local operating-system representation of a file
	 * name.
	 */
	var LocalPath(default, never):String;
	/**
	 * Gets the original URI string that was passed to the  constructor.
	 * @return A  containing the exact URI specified when this instance was
	 * constructed; otherwise, .
	 */
	var OriginalString(default, never):String;
	/**
	 * Gets the  and  properties separated by a question mark (?).
	 * @return A  that contains the  and  properties separated by a question mark (?).
	 */
	var PathAndQuery(default, never):String;
	/**
	 * Gets the port number of this URI.
	 * @return An  value that contains the port number for this URI.
	 */
	var Port(default, never):Int;
	/**
	 * Gets any query information included in the specified URI.
	 * @return A  that contains any query information included in the specified URI.
	 */
	var Query(default, never):String;
	/**
	 * Gets the scheme name for this URI.
	 * @return A  that contains the scheme for this URI, converted to lowercase.
	 */
	var Scheme(default, never):String;
	/**
	 * Gets an array containing the path segments that make up the specified URI.
	 * @return A  array that contains the path segments that make up the specified URI.
	 */
	var Segments(default, never):cs.NativeArray<String>;
	/**
	 * Indicates that the URI string was completely escaped before the  instance was
	 * created.
	 * @return A  value that is  if the  parameter was set to  when the  instance was
	 * created; otherwise, .
	 */
	var UserEscaped(default, never):Bool;
	/**
	 * Gets the user name, password, or other user-specific information associated with
	 * the specified URI.
	 * @return A  that contains the user information associated with the URI. The
	 * returned value does not include the '@' character reserved for delimiting the
	 * user information part of the URI.
	 */
	var UserInfo(default, never):String;
	@:overload(function(uriString:String):Void {})
	@:overload(function(uriString:String, dontEscape:Bool):Void {})
	@:overload(function(uriString:String, uriKind:cs.system.UriKind):Void {})
	@:overload(function(baseUri:cs.system.Uri, relativeUri:String):Void {})
	@:overload(function(baseUri:cs.system.Uri, relativeUri:cs.system.Uri):Void {})
	function new(baseUri:cs.system.Uri, relativeUri:String, dontEscape:Bool):Void;
	/**
	 * Determines whether the specified host name is a valid DNS name.
	 * @param name The host name to validate. This can be an IPv4 or IPv6 address or an
	 * Internet host name.
	 * @return A  that indicates the type of the host name. If the type of the host
	 * name cannot be determined or if the host name is  or a zero-length string, this
	 * method returns .
	 */
	static function CheckHostName(name:String):cs.system.UriHostNameType;
	/**
	 * Determines whether the specified scheme name is valid.
	 * @param schemeName The scheme name to validate.
	 * @return A  value that is  if the scheme name is valid; otherwise, .
	 */
	static function CheckSchemeName(schemeName:String):Bool;
	/**
	 * Compares the specified parts of two URIs using the specified comparison rules.
	 * @param uri1 The first .
	 * @param uri2 The second .
	 * @param partsToCompare A bitwise combination of the  values that specifies the
	 * parts of  and  to compare.
	 * @param compareFormat One of the  values that specifies the character escaping
	 * used when the URI components are compared.
	 * @param comparisonType One of the  values.
	 * @return An  value that indicates the lexical relationship between the compared 
	 * components. Value Meaning Less than zero is less than . Zero equals . Greater
	 * than zero is greater than .
	 */
	static function Compare(uri1:cs.system.Uri, uri2:cs.system.Uri, partsToCompare:cs.system.UriComponents, compareFormat:cs.system.UriFormat, comparisonType:cs.system.StringComparison):Int;
	/**
	 * Converts a string to its escaped representation.
	 * @param stringToEscape The string to escape.
	 * @return A  that contains the escaped representation of .
	 */
	static function EscapeDataString(stringToEscape:String):String;
	/**
	 * Converts a URI string to its escaped representation.
	 * @param stringToEscape The string to escape.
	 * @return A  that contains the escaped representation of .
	 */
	static function EscapeUriString(stringToEscape:String):String;
	/**
	 * Gets the decimal value of a hexadecimal digit.
	 * @param digit The hexadecimal digit (0-9, a-f, A-F) to convert.
	 * @return An  value that contains a number from 0 to 15 that corresponds to the
	 * specified hexadecimal digit.
	 */
	static function FromHex(digit:cs.Char16):Int;
	/**
	 * Converts a specified character into its hexadecimal equivalent.
	 * @param character The character to convert to hexadecimal representation.
	 * @return The hexadecimal representation of the specified character.
	 */
	static function HexEscape(character:cs.Char16):String;
	/**
	 * Converts a specified hexadecimal representation of a character to the character.
	 * @param pattern The hexadecimal representation of a character.
	 * @param index The location in  where the hexadecimal representation of a
	 * character begins.
	 * @return The character represented by the hexadecimal encoding at position . If
	 * the character at  is not hexadecimal encoded, the character at  is returned. The
	 * value of  is incremented to point to the character following the one returned.
	 */
	static function HexUnescape(pattern:String, index:cs.Ref<Int>):cs.Char16;
	/**
	 * Determines whether a specified character is a valid hexadecimal digit.
	 * @param character The character to validate.
	 * @return if the character is a valid hexadecimal digit; otherwise, .
	 */
	static function IsHexDigit(character:cs.Char16):Bool;
	/**
	 * Determines whether a character in a string is hexadecimal encoded.
	 * @param pattern The string to check.
	 * @param index The location in  to check for hexadecimal encoding.
	 * @return A  value that is  if  is hexadecimal encoded at the specified location;
	 * otherwise, .
	 */
	static function IsHexEncoding(pattern:String, index:Int):Bool;
	/**
	 * Indicates whether the string is well-formed by attempting to construct a URI
	 * with the string and ensures that the string does not require further escaping.
	 * @param uriString The string used to attempt to construct a .
	 * @param uriKind The type of the  in .
	 * @return if the string was well-formed; otherwise, .
	 */
	static function IsWellFormedUriString(uriString:String, uriKind:cs.system.UriKind):Bool;
	/**
	 * Determines whether two  instances have the same value.
	 * @param uri1 A  instance to compare with .
	 * @param uri2 A  instance to compare with .
	 * @return A  value that is  if the  instances are equivalent; otherwise, .
	 */
	static function op_Equality(uri1:cs.system.Uri, uri2:cs.system.Uri):Bool;
	/**
	 * Determines whether two  instances do not have the same value.
	 * @param uri1 A  instance to compare with .
	 * @param uri2 A  instance to compare with .
	 * @return A  value that is  if the two  instances are not equal; otherwise, . If
	 * either parameter is , this method returns .
	 */
	static function op_Inequality(uri1:cs.system.Uri, uri2:cs.system.Uri):Bool;
	@:overload(function(uriString:String, uriKind:cs.system.UriKind, result:cs.Ref<cs.system.Uri>):Bool {})
	@:overload(function(baseUri:cs.system.Uri, relativeUri:String, result:cs.Ref<cs.system.Uri>):Bool {})
	/**
	 * Creates a new  using the specified  instance and a .
	 * @param uriString The  representing the .
	 * @param uriKind The type of the Uri.
	 * @param result When this method returns, contains the constructed .
	 * @return A  value that is  if the  was successfully created; otherwise, .
	 */
	static function TryCreate(baseUri:cs.system.Uri, relativeUri:cs.system.Uri, result:cs.Ref<cs.system.Uri>):Bool;
	/**
	 * Converts a string to its unescaped representation.
	 * @param stringToUnescape The string to unescape.
	 * @return A  that contains the unescaped representation of .
	 */
	static function UnescapeDataString(stringToUnescape:String):String;
	/**
	 * Compares two  instances for equality.
	 * @param comparand The  instance or a URI identifier to compare with the current
	 * instance.
	 * @return A  value that is  if the two instances represent the same URI;
	 * otherwise, .
	 */
	function Equals(comparand:Dynamic):Bool;
	/**
	 * Gets the specified components of the current instance using the specified
	 * escaping for special characters.
	 * @param components A bitwise combination of the  values that specifies which
	 * parts of the current instance to return to the caller.
	 * @param format One of the  values that controls how special characters are
	 * escaped.
	 * @return A  that contains the components.
	 */
	function GetComponents(components:cs.system.UriComponents, format:cs.system.UriFormat):String;
	/**
	 * Gets the hash code for the URI.
	 * @return An  containing the hash value generated for this URI.
	 */
	function GetHashCode():Int;
	/**
	 * Gets the specified portion of a  instance.
	 * @param part One of the  values that specifies the end of the URI portion to
	 * return.
	 * @return A  that contains the specified portion of the  instance.
	 */
	function GetLeftPart(part:cs.system.UriPartial):String;
	/**
	 * Determines whether the current  instance is a base of the specified  instance.
	 * @param uri The specified  instance to test.
	 * @return if the current  instance is a base of ; otherwise, .
	 */
	function IsBaseOf(uri:cs.system.Uri):Bool;
	/**
	 * Indicates whether the string used to construct this  was well-formed and is not
	 * required to be further escaped.
	 * @return if the string was well-formed; otherwise, .
	 */
	function IsWellFormedOriginalString():Bool;
	/**
	 * Determines the difference between two  instances.
	 * @param toUri The URI to compare to the current URI.
	 * @return If the hostname and scheme of this URI instance and  are the same, then
	 * this method returns a  that represents a relative URI that, when appended to the
	 * current URI instance, yields the  parameter. If the hostname or scheme is
	 * different, then this method returns a  that represents the  parameter.
	 */
	function MakeRelative(toUri:cs.system.Uri):String;
	/**
	 * Determines the difference between two  instances.
	 * @param uri The URI to compare to the current URI.
	 * @return If the hostname and scheme of this URI instance and  are the same, then
	 * this method returns a relative  that, when appended to the current URI instance,
	 * yields . If the hostname or scheme is different, then this method returns a 
	 * that represents the  parameter.
	 */
	function MakeRelativeUri(uri:cs.system.Uri):cs.system.Uri;
	/**
	 * Gets a canonical string representation for the specified  instance.
	 * @return A  instance that contains the unescaped canonical representation of the 
	 * instance. All characters are unescaped except #, ?, and %.
	 */
	function ToString():String;
}
