package cs.system;

/** Controls how URI information is escaped. */
@:native("System.UriFormat")
extern enum UriFormat {
	SafeUnescaped;
	Unescaped;
	UriEscaped;
}
