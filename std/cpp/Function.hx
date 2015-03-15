package cpp;

@:coreType @:structAccess @:include("cpp/Pointer.h")
@:analyzer(no_simplification)
extern class Function<T,ABI:cpp.abi.Abi>
{
   public function new(d:Dynamic);

   // Actually a function pointer, but can be called using haxe notation
	public var call(default,null):T;

   public static function getProcAddress<T,ABI:cpp.abi.Abi>(inModule:String, inFunction:String) : Function<T,ABI>;
   public static function fromStaticFunction<T>(inStaticFunction:T) : Callable<T>;

	public function lt(inOther:Function<T,ABI>):Bool;
	public function leq(inOther:Function<T,ABI>):Bool;
	public function gt(inOther:Function<T,ABI>):Bool;
	public function geq(inOther:Function<T,ABI>):Bool;
}



