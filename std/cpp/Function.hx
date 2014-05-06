package cpp;

@:coreType @:include("cpp/Pointer.h") @:native("cpp.Pointer")
extern class Function<T> extends BasePointer<T>
{
   public static function getProcAddress<T>(inModule:String, inFunction:String) : Function<T>;
   public static function fromStaticFunction<T>(inStaticFunction:T) : Function<T>;
}


