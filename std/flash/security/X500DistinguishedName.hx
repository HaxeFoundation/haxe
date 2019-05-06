package flash.security;

extern class X500DistinguishedName {
	@:flash.property var commonName(get,never) : String;
	@:flash.property var countryName(get,never) : String;
	@:flash.property var localityName(get,never) : String;
	@:flash.property var organizationName(get,never) : String;
	@:flash.property var organizationalUnitName(get,never) : String;
	@:flash.property var stateOrProvinceName(get,never) : String;
	function new() : Void;
	private function get_commonName() : String;
	private function get_countryName() : String;
	private function get_localityName() : String;
	private function get_organizationName() : String;
	private function get_organizationalUnitName() : String;
	private function get_stateOrProvinceName() : String;
	function toString() : String;
}
