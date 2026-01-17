package cs.system;

@:native("System.DateTimeKind")
extern enum abstract DateTimeKind(Int) {
	var Unspecified;
	var Utc;
	var Local;
}
