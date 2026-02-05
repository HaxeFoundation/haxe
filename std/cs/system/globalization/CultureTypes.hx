package cs.system.globalization;

/** Defines the types of culture lists that can be retrieved using the  method. */
@:native("System.Globalization.CultureTypes")
extern enum abstract CultureTypes(Int) {
	var AllCultures = 7;
	var FrameworkCultures = 64;
	var InstalledWin32Cultures = 4;
	var NeutralCultures = 1;
	var ReplacementCultures = 16;
	var SpecificCultures = 2;
	var UserCustomCulture = 8;
	var WindowsOnlyCultures = 32;
	@:op(A | B) static function or(lhs:CultureTypes, rhs:CultureTypes):CultureTypes;
	@:op(A & B) static function and(lhs:CultureTypes, rhs:CultureTypes):CultureTypes;
	@:op(A ^ B) static function xor(lhs:CultureTypes, rhs:CultureTypes):CultureTypes;
	@:op(~A) static function complement(value:CultureTypes):CultureTypes;
}
