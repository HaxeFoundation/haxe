package cs.system.reflection;

/** Specifies the resource location. */
@:native("System.Reflection.ResourceLocation")
extern enum abstract ResourceLocation(Int) {
	var ContainedInAnotherAssembly = 2;
	var ContainedInManifestFile = 4;
	var Embedded = 1;
	@:op(A | B) static function or(lhs:ResourceLocation, rhs:ResourceLocation):ResourceLocation;
	@:op(A & B) static function and(lhs:ResourceLocation, rhs:ResourceLocation):ResourceLocation;
	@:op(A ^ B) static function xor(lhs:ResourceLocation, rhs:ResourceLocation):ResourceLocation;
	@:op(~A) static function complement(value:ResourceLocation):ResourceLocation;
}
