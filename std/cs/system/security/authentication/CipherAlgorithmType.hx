package cs.system.security.authentication;

/** Defines the possible cipher algorithms for the  class. */
@:native("System.Security.Authentication.CipherAlgorithmType")
extern enum CipherAlgorithmType {
	Aes;
	Aes128;
	Aes192;
	Aes256;
	Des;
	None;
	Null;
	Rc2;
	Rc4;
	TripleDes;
}
