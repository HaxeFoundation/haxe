package cs.system;

/** Defines the different kinds of URIs. */
@:native("System.UriKind")
extern enum abstract UriKind(Int) {
	var Absolute = 1;
	var Relative = 2;
	var RelativeOrAbsolute = 0;
}
