package cs.system.reflection.emit;

/** Describes the types of the Microsoft intermediate language (MSIL) instructions. */
@:native("System.Reflection.Emit.OpCodeType")
extern enum OpCodeType {
	Annotation;
	Macro;
	Nternal;
	Objmodel;
	Prefix;
	Primitive;
}
