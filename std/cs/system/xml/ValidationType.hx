package cs.system.xml;

/** Specifies the type of validation to perform. */
@:native("System.Xml.ValidationType")
extern enum abstract ValidationType(Int) {
	var Auto = 1;
	var DTD = 2;
	var None = 0;
	var Schema = 4;
	var XDR = 3;
}
