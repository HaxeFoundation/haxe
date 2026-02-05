package cs.system;

/** Defines the different kinds of URIs. */
@:native("System.UriKind")
extern enum UriKind {
	Absolute;
	Relative;
	RelativeOrAbsolute;
}
