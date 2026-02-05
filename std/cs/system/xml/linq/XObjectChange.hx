package cs.system.xml.linq;

/** Specifies the event type when an event is raised for an . */
@:native("System.Xml.Linq.XObjectChange")
extern enum abstract XObjectChange(Int) {
	var Add = 0;
	var Name = 2;
	var Remove = 1;
	var Value = 3;
}
