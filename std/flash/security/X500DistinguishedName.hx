package flash.security;

extern class X500DistinguishedName {
	var commonName(default,never) : String;
	var countryName(default,never) : String;
	var localityName(default,never) : String;
	var organizationName(default,never) : String;
	var organizationalUnitName(default,never) : String;
	var stateOrProvinceName(default,never) : String;
	function new() : Void;
	function toString() : String;
}
