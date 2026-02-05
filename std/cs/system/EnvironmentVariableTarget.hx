package cs.system;

/** Specifies the location where an environment variable is stored or retrieved in a set or get operation. */
@:native("System.EnvironmentVariableTarget")
extern enum abstract EnvironmentVariableTarget(Int) {
	var Machine = 2;
	var Process = 0;
	var User = 1;
}
