package php;

/**
    Special type which allows passing function arguments by reference.
    This type should be used for externs only.
**/
abstract Ref<T>(T) from T to T {

}