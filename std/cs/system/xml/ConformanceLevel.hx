package cs.system.xml;

/** Specifies the amount of input or output checking that  and  objects perform. */
@:native("System.Xml.ConformanceLevel")
extern enum abstract ConformanceLevel(Int) {
	var Auto = 0;
	var Document = 2;
	var Fragment = 1;
}
