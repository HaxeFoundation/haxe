package cpp;

@:native("::hx::Object *")
extern class HxObjectPtr
{
   @:native("hx::DynamicPtr")
   static function fromDynamic(x:Dynamic):Object;
   @:native("Dynamic")
   static function toDynamic(x:Object):Dynamic;
}

@:extern
abstract Object(HxObjectPtr) {
	@:from public inline static function from(x:Dynamic):Object return HxObjectPtr.fromDynamic(x);
	@:to public inline static function to(inVal:HxObjectPtr):Dynamic return HxObjectPtr.toDynamic(inVal);
}
