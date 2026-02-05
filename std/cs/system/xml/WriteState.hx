package cs.system.xml;

/** Specifies the state of the . */
@:native("System.Xml.WriteState")
extern enum WriteState {
	Attribute;
	Closed;
	Content;
	Element;
	Error;
	Prolog;
	Start;
}
