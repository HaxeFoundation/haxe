package cs.system;

/** Specifies the location where an environment variable is stored or retrieved in a set or get operation. */
@:native("System.EnvironmentVariableTarget")
extern enum EnvironmentVariableTarget {
	Machine;
	Process;
	User;
}
