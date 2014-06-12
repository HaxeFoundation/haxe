package cpp;

@:native("::hx::Object *")
extern class HxObjectPtr
{
   @:native("hx::DynamicPtr")
   static function fromDynamic(x:Dynamic):Object;
   @:native("Dynamic")
   static function toDynamic(x:Object):Dynamic;
}

abstract Object(HxObjectPtr) {
	@:from public inline static function from(x:Dynamic):Object return HxObjectPtr.fromDynamic(x);
	@:to public inline function to():Dynamic return HxObjectPtr.toDynamic(this);
}
