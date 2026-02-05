package cs.system;

/** Controls how URI information is escaped. */
@:native("System.UriFormat")
extern enum abstract UriFormat(Int) {
	var SafeUnescaped = 3;
	var Unescaped = 2;
	var UriEscaped = 1;
}
