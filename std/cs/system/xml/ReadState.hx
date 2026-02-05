package cs.system.xml;

/** Specifies the state of the reader. */
@:native("System.Xml.ReadState")
extern enum ReadState {
	Closed;
	EndOfFile;
	Error;
	Initial;
	Interactive;
}
