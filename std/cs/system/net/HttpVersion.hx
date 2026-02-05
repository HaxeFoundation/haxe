package cs.system.net;

/** Defines the HTTP version numbers that are supported by the  and  classes. */
@:native("System.Net.HttpVersion")
extern class HttpVersion {
	static var Unknown(default, never):cs.system.Version;
	/** Defines a  instance for HTTP 1.0. */
	static var Version10(default, never):cs.system.Version;
	/** Defines a  instance for HTTP 1.1. */
	static var Version11(default, never):cs.system.Version;
	static var Version20(default, never):cs.system.Version;
}
