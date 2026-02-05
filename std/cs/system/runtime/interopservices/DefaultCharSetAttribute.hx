package cs.system.runtime.interopservices;

/** Specifies the value of the  enumeration. This class cannot be inherited. */
@:native("System.Runtime.InteropServices.DefaultCharSetAttribute")
extern class DefaultCharSetAttribute extends cs.system.Attribute {
	/**
	 * Gets the default value of  for any call to .
	 * @return The default value of  for any call to .
	 */
	var CharSet(default, never):cs.system.runtime.interopservices.CharSet;
	function new(charSet:cs.system.runtime.interopservices.CharSet):Void;
}
