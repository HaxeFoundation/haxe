package flash.security;

extern class X509Certificate {
	var encoded(get,never) : flash.utils.ByteArray;
	var issuer(get,never) : X500DistinguishedName;
	var issuerUniqueID(get,never) : String;
	var serialNumber(get,never) : String;
	var signatureAlgorithmOID(get,never) : String;
	var signatureAlgorithmParams(get,never) : flash.utils.ByteArray;
	var subject(get,never) : X500DistinguishedName;
	var subjectPublicKey(get,never) : String;
	var subjectPublicKeyAlgorithmOID(get,never) : String;
	var subjectUniqueID(get,never) : String;
	var validNotAfter(get,never) : Date;
	var validNotBefore(get,never) : Date;
	var version(get,never) : UInt;
	function new() : Void;
	private function get_encoded() : flash.utils.ByteArray;
	private function get_issuer() : X500DistinguishedName;
	private function get_issuerUniqueID() : String;
	private function get_serialNumber() : String;
	private function get_signatureAlgorithmOID() : String;
	private function get_signatureAlgorithmParams() : flash.utils.ByteArray;
	private function get_subject() : X500DistinguishedName;
	private function get_subjectPublicKey() : String;
	private function get_subjectPublicKeyAlgorithmOID() : String;
	private function get_subjectUniqueID() : String;
	private function get_validNotAfter() : Date;
	private function get_validNotBefore() : Date;
	private function get_version() : UInt;
}
