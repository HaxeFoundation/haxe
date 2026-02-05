package cs.system;

/** A customizable parser for a hierarchical URI. */
@:native("System.GenericUriParser")
extern class GenericUriParser extends cs.system.UriParser {
	function new(options:cs.system.GenericUriParserOptions):Void;
}
