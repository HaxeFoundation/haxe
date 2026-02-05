package cs.system.xml.linq;

/** Specifies the event type when an event is raised for an . */
@:native("System.Xml.Linq.XObjectChange")
extern enum XObjectChange {
	Add;
	Name;
	Remove;
	Value;
}
