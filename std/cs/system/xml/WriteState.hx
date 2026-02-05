package cs.system.xml;

/** Specifies the state of the . */
@:native("System.Xml.WriteState")
extern enum abstract WriteState(Int) {
	var Attribute = 3;
	var Closed = 5;
	var Content = 4;
	var Element = 2;
	var Error = 6;
	var Prolog = 1;
	var Start = 0;
}
