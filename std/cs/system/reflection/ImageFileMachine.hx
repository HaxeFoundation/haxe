package cs.system.reflection;

/** Identifies the platform targeted by an executable. */
@:native("System.Reflection.ImageFileMachine")
extern enum abstract ImageFileMachine(Int) {
	var AMD64 = 34404;
	var ARM = 452;
	var I386 = 332;
	var IA64 = 512;
}
