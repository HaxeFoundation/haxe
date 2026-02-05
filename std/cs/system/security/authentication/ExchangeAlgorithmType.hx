package cs.system.security.authentication;

/** Specifies the algorithm used to create keys shared by the client and server. */
@:native("System.Security.Authentication.ExchangeAlgorithmType")
extern enum ExchangeAlgorithmType {
	DiffieHellman;
	None;
	RsaKeyX;
	RsaSign;
}
