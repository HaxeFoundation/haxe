package cs;

/**
	This type represents "out" types for C# function parameters. 
	It only has effect on function parameters, and conversion to/from the referenced type is automatic.
	
	Note: Using this type should be considered a bad practice unless overriding a native function is needed.
**/
typedef Out<T> = T;