package cs.system.io;

/** Changes that might occur to a file or directory. */
@:native("System.IO.WatcherChangeTypes")
extern enum abstract WatcherChangeTypes(Int) {
	var All = 15;
	var Changed = 4;
	var Created = 1;
	var Deleted = 2;
	var Renamed = 8;
	@:op(A | B) static function or(lhs:WatcherChangeTypes, rhs:WatcherChangeTypes):WatcherChangeTypes;
	@:op(A & B) static function and(lhs:WatcherChangeTypes, rhs:WatcherChangeTypes):WatcherChangeTypes;
	@:op(A ^ B) static function xor(lhs:WatcherChangeTypes, rhs:WatcherChangeTypes):WatcherChangeTypes;
	@:op(~A) static function complement(value:WatcherChangeTypes):WatcherChangeTypes;
}
