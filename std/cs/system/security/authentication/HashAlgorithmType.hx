package cs.system.security.authentication;

/** Specifies the algorithm used for generating message authentication codes (MACs). */
@:native("System.Security.Authentication.HashAlgorithmType")
extern enum HashAlgorithmType {
	Md5;
	None;
	Sha1;
	Sha256;
	Sha384;
	Sha512;
}
