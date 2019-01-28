package haxe;

/**
	Special type which is handled in null safety checks to force using of nullable values as not-nullable.
	Nullable values will be passed to/from this type without any checks.
	Also expressions like `(expr:Unsafe<T>)` are not checked for null safety.
**/
typedef Unsafe<T> = T;