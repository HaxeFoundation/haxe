package cs.system.threading;

/**
	Extern for System.Threading.Volatile class providing volatile read/write operations.
**/
@:native("System.Threading.Volatile")
extern class Volatile {
	/**
		Reads the value of a field. On systems that require it, inserts a memory barrier
		that prevents the processor from reordering memory operations.
	**/
	@:overload(function<T:{}>(location:cs.Ref<T>):T {})
	@:overload(function(location:cs.Ref<Int>):Int {})
	static function Read(location:Int):Int;

	/**
		Writes the specified value to a field. On systems that require it, inserts a memory barrier
		that prevents the processor from reordering memory operations.
	**/
	@:overload(function<T:{}>(location:cs.Ref<T>, value:T):Void {})
	@:overload(function(location:cs.Ref<Int>, value:Int):Void {})
	static function Write(location:Int, value:Int):Void;
}
