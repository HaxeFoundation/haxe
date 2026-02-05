package cs.system.xml;

/** Specifies how the  or  handle entities. */
@:native("System.Xml.EntityHandling")
extern enum abstract EntityHandling(Int) {
	var ExpandCharEntities = 2;
	var ExpandEntities = 1;
}
