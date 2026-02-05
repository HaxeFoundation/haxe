package cs.system.security.cryptography;

/** Specifies whether white space should be ignored in the base 64 transformation. */
@:native("System.Security.Cryptography.FromBase64TransformMode")
extern enum FromBase64TransformMode {
	DoNotIgnoreWhiteSpaces;
	IgnoreWhiteSpaces;
}
