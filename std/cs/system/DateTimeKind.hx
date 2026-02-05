package cs.system;

/** Specifies whether a  object represents a local time, a Coordinated Universal Time (UTC), or is not specified as either local time or UTC. */
@:native("System.DateTimeKind")
extern enum abstract DateTimeKind(Int) {
	var Local = 2;
	var Unspecified = 0;
	var Utc = 1;
}
