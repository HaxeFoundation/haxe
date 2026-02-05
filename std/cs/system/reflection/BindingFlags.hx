package cs.system.reflection;

/** Specifies flags that control binding and the way in which the search for members and types is conducted by reflection. */
@:native("System.Reflection.BindingFlags")
extern enum abstract BindingFlags(Int) {
	var CreateInstance = 512;
	var DeclaredOnly = 2;
	var Default = 0;
	var DoNotWrapExceptions = 33554432;
	var ExactBinding = 65536;
	var FlattenHierarchy = 64;
	var GetField = 1024;
	var GetProperty = 4096;
	var IgnoreCase = 1;
	var IgnoreReturn = 16777216;
	var Instance = 4;
	var InvokeMethod = 256;
	var NonPublic = 32;
	var OptionalParamBinding = 262144;
	var Public = 16;
	var PutDispProperty = 16384;
	var PutRefDispProperty = 32768;
	var SetField = 2048;
	var SetProperty = 8192;
	var Static = 8;
	var SuppressChangeType = 131072;
	@:op(A | B) static function or(lhs:BindingFlags, rhs:BindingFlags):BindingFlags;
	@:op(A & B) static function and(lhs:BindingFlags, rhs:BindingFlags):BindingFlags;
	@:op(A ^ B) static function xor(lhs:BindingFlags, rhs:BindingFlags):BindingFlags;
	@:op(~A) static function complement(value:BindingFlags):BindingFlags;
}
