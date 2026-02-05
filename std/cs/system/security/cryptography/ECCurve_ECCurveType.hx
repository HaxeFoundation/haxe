package cs.system.security.cryptography;

@:native("System.Security.Cryptography.ECCurve.ECCurveType")
extern enum abstract ECCurve_ECCurveType(Int) {
	var Characteristic2 = 4;
	var Implicit = 0;
	var Named = 5;
	var PrimeMontgomery = 3;
	var PrimeShortWeierstrass = 1;
	var PrimeTwistedEdwards = 2;
}
