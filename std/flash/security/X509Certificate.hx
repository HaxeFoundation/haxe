package flash.security;

extern class X509Certificate {
	var encoded(default,null) : flash.utils.ByteArray;
	var issuer(default,null) : X500DistinguishedName;
	var issuerUniqueID(default,null) : String;
	var serialNumber(default,null) : String;
	var signatureAlgorithmOID(default,null) : String;
	var signatureAlgorithmParams(default,null) : flash.utils.ByteArray;
	var subject(default,null) : X500DistinguishedName;
	var subjectPublicKey(default,null) : String;
	var subjectPublicKeyAlgorithmOID(default,null) : String;
	var subjectUniqueID(default,null) : String;
	var validNotAfter(default,null) : Date;
	var validNotBefore(default,null) : Date;
	var version(default,null) : UInt;
	function new() : Void;
}
