package flash.security;

extern class X500DistinguishedName {
	var commonName(default,null) : String;
	var countryName(default,null) : String;
	var localityName(default,null) : String;
	var organizationName(default,null) : String;
	var organizationalUnitName(default,null) : String;
	var stateOrProvinceName(default,null) : String;
	function new() : Void;
	function toString() : String;
}
