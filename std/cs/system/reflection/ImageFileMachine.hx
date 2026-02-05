package cs.system.reflection;

/** Identifies the platform targeted by an executable. */
@:native("System.Reflection.ImageFileMachine")
extern enum ImageFileMachine {
	AMD64;
	ARM;
	I386;
	IA64;
}
