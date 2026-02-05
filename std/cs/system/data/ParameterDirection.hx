package cs.system.data;

/** Specifies the type of a parameter within a query relative to the . */
@:native("System.Data.ParameterDirection")
extern enum ParameterDirection {
	Input;
	InputOutput;
	Output;
	ReturnValue;
}
