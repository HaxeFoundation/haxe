package cs.system.reflection;

/** Provides information about the type of code contained in an assembly. */
@:native("System.Reflection.AssemblyContentType")
extern enum abstract AssemblyContentType(Int) {
	var Default = 0;
	var WindowsRuntime = 1;
}
