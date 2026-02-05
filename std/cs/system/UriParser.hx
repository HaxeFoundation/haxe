package cs.system;

/** Parses a new URI scheme. This is an abstract class. */
@:native("System.UriParser")
extern class UriParser {
	/**
	 * Indicates whether the parser for a scheme is registered.
	 * @param schemeName The scheme name to check.
	 * @return if  has been registered; otherwise, .
	 */
	static function IsKnownScheme(schemeName:String):Bool;
	/**
	 * Associates a scheme and port number with a .
	 * @param uriParser The URI parser to register.
	 * @param schemeName The name of the scheme that is associated with this parser.
	 * @param defaultPort The default port number for the specified scheme.
	 */
	static function Register(uriParser:cs.system.UriParser, schemeName:String, defaultPort:Int):Void;
}
