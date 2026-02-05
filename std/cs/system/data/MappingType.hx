package cs.system.data;

/** Specifies how a  is mapped. */
@:native("System.Data.MappingType")
extern enum abstract MappingType(Int) {
	var Attribute = 2;
	var Element = 1;
	var Hidden = 4;
	var SimpleContent = 3;
}
