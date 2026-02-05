package cs.system.xml;

/** Specifies the state of the reader. */
@:native("System.Xml.ReadState")
extern enum abstract ReadState(Int) {
	var Closed = 4;
	var EndOfFile = 3;
	var Error = 2;
	var Initial = 0;
	var Interactive = 1;
}
