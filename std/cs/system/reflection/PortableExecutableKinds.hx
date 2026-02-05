package cs.system.reflection;

/** Identifies the nature of the code in an executable file. */
@:native("System.Reflection.PortableExecutableKinds")
extern enum abstract PortableExecutableKinds(Int) {
	var ILOnly = 1;
	var NotAPortableExecutableImage = 0;
	var PE32Plus = 4;
	var Preferred32Bit = 16;
	var Required32Bit = 2;
	var Unmanaged32Bit = 8;
	@:op(A | B) static function or(lhs:PortableExecutableKinds, rhs:PortableExecutableKinds):PortableExecutableKinds;
	@:op(A & B) static function and(lhs:PortableExecutableKinds, rhs:PortableExecutableKinds):PortableExecutableKinds;
	@:op(A ^ B) static function xor(lhs:PortableExecutableKinds, rhs:PortableExecutableKinds):PortableExecutableKinds;
	@:op(~A) static function complement(value:PortableExecutableKinds):PortableExecutableKinds;
}
