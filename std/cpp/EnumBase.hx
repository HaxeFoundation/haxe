package cpp;

@:native("hx.EnumBase")
extern class EnumBase
{
   #if (hxcpp_api_level >= 330)
   public function getIndex():Int;
   public function getTag():String;
   public function getParamCount():Int;
   public function getParamI(inIndex:Int):Dynamic;
   public function getParameters():Array<Dynamic>;
   #else
   public function __EnumParams():Array<Dynamic>;
   public function __Tag():String;
   public function __Index():Int;

   inline public function getIndex():Int  return untyped __Index();
   inline public function getTag():String  return untyped __Tag();
   inline public function getParamCount():Int return untyped __EnumParams()==null ? 0 : __EnumParams().length;
   inline public function getParamI(inIndex:Int):Dynamic return untyped __EnumParams()[inIndex];
   inline public function getParameters():Array<Dynamic> return __EnumParams()==null ? [] : __EnumParams();

   #end
}

