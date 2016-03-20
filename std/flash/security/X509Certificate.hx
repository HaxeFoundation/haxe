package flash.security;

extern class X509Certificate {
	var encoded(default,never) : flash.utils.ByteArray;
	var issuer(default,never) : X500DistinguishedName;
	var issuerUniqueID(default,never) : String;
	var serialNumber(default,never) : String;
	var signatureAlgorithmOID(default,never) : String;
	var signatureAlgorithmParams(default,never) : flash.utils.ByteArray;
	var subject(default,never) : X500DistinguishedName;
	var subjectPublicKey(default,never) : String;
	var subjectPublicKeyAlgorithmOID(default,never) : String;
	var subjectUniqueID(default,never) : String;
	var validNotAfter(default,never) : Date;
	var validNotBefore(default,never) : Date;
	var version(default,never) : UInt;
	function new() : Void;
}
