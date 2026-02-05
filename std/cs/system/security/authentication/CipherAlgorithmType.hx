package cs.system.security.authentication;

/** Defines the possible cipher algorithms for the  class. */
@:native("System.Security.Authentication.CipherAlgorithmType")
extern enum abstract CipherAlgorithmType(Int) {
	var Aes = 26129;
	var Aes128 = 26126;
	var Aes192 = 26127;
	var Aes256 = 26128;
	var Des = 26113;
	var None = 0;
	var Null = 24576;
	var Rc2 = 26114;
	var Rc4 = 26625;
	var TripleDes = 26115;
}
