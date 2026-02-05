package cs.system.security.authentication;

/** Specifies the algorithm used for generating message authentication codes (MACs). */
@:native("System.Security.Authentication.HashAlgorithmType")
extern enum abstract HashAlgorithmType(Int) {
	var Md5 = 32771;
	var None = 0;
	var Sha1 = 32772;
	var Sha256 = 32780;
	var Sha384 = 32781;
	var Sha512 = 32782;
}
