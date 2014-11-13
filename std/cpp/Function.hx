package cpp;

@:coreType @:structAccess @:include("cpp/Pointer.h")
@:analyzer(no_simplification)
extern class Function<T>
{
   public function new(d:Dynamic);

   // Actually a function pointer, but can be called using haxe notation
	public var call(default,null):T;

   public static function getProcAddress<T>(inModule:String, inFunction:String) : Function<T>;
   public static function fromStaticFunction<T>(inStaticFunction:T) : Function<T>;

	public function lt(inOther:Function<T>):Bool;
	public function leq(inOther:Function<T>):Bool;
	public function gt(inOther:Function<T>):Bool;
	public function geq(inOther:Function<T>):Bool;


}


