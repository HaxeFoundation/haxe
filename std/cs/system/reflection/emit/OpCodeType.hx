package cs.system.reflection.emit;

/** Describes the types of the Microsoft intermediate language (MSIL) instructions. */
@:native("System.Reflection.Emit.OpCodeType")
extern enum abstract OpCodeType(Int) {
	var Annotation = 0;
	var Macro = 1;
	var Nternal = 2;
	var Objmodel = 3;
	var Prefix = 4;
	var Primitive = 5;
}
