package cs.system.security.authentication;

/** Specifies the algorithm used to create keys shared by the client and server. */
@:native("System.Security.Authentication.ExchangeAlgorithmType")
extern enum abstract ExchangeAlgorithmType(Int) {
	var DiffieHellman = 43522;
	var None = 0;
	var RsaKeyX = 41984;
	var RsaSign = 9216;
}
