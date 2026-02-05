package cs.system.security.cryptography;

@:native("System.Security.Cryptography.ECCurve.ECCurveType")
extern enum ECCurve_ECCurveType {
	Characteristic2;
	Implicit;
	Named;
	PrimeMontgomery;
	PrimeShortWeierstrass;
	PrimeTwistedEdwards;
}
