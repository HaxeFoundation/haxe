package cs.system.security.cryptography;

/** Specifies whether white space should be ignored in the base 64 transformation. */
@:native("System.Security.Cryptography.FromBase64TransformMode")
extern enum abstract FromBase64TransformMode(Int) {
	var DoNotIgnoreWhiteSpaces = 1;
	var IgnoreWhiteSpaces = 0;
}
