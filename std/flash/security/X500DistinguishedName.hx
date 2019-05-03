package flash.security;

extern class X500DistinguishedName {
	var commonName(get,never) : String;
	var countryName(get,never) : String;
	var localityName(get,never) : String;
	var organizationName(get,never) : String;
	var organizationalUnitName(get,never) : String;
	var stateOrProvinceName(get,never) : String;
	function new() : Void;
	private function get_commonName() : String;
	private function get_countryName() : String;
	private function get_localityName() : String;
	private function get_organizationName() : String;
	private function get_organizationalUnitName() : String;
	private function get_stateOrProvinceName() : String;
	function toString() : String;
}
