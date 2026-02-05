package cs.system.io.isolatedstorage;

/** Enumerates the levels of isolated storage scope that are supported by . */
@:native("System.IO.IsolatedStorage.IsolatedStorageScope")
extern enum abstract IsolatedStorageScope(Int) {
	var Application = 32;
	var Assembly = 4;
	var Domain = 2;
	var Machine = 16;
	var None = 0;
	var Roaming = 8;
	var User = 1;
	@:op(A | B) static function or(lhs:IsolatedStorageScope, rhs:IsolatedStorageScope):IsolatedStorageScope;
	@:op(A & B) static function and(lhs:IsolatedStorageScope, rhs:IsolatedStorageScope):IsolatedStorageScope;
	@:op(A ^ B) static function xor(lhs:IsolatedStorageScope, rhs:IsolatedStorageScope):IsolatedStorageScope;
	@:op(~A) static function complement(value:IsolatedStorageScope):IsolatedStorageScope;
}
