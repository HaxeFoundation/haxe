package cs.system.xml;

/** Specifies how white space is handled. */
@:native("System.Xml.WhitespaceHandling")
extern enum abstract WhitespaceHandling(Int) {
	var All = 0;
	var None = 2;
	var Significant = 1;
}
