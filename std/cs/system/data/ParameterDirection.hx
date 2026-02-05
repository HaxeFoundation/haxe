package cs.system.data;

/** Specifies the type of a parameter within a query relative to the . */
@:native("System.Data.ParameterDirection")
extern enum abstract ParameterDirection(Int) {
	var Input = 1;
	var InputOutput = 3;
	var Output = 2;
	var ReturnValue = 6;
}
