package php;

/**
	Special type which allows passing function arguments by reference.
	This type should be used for externs only.
**/
@:analyzer(as_var)
typedef Ref<T> = T;