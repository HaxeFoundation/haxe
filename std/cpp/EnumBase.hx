package cpp;

@:native("hx.EnumBase")
extern class EnumBase
{
   #if (hxcpp_api_level >= 330)
   public function _hx_getIndex():Int;
   public function _hx_getTag():String;
   public function _hx_getParamCount():Int;
   public function _hx_getParamI(inIndex:Int):Dynamic;
   public function _hx_getParameters():Array<Dynamic>;

   inline public function getIndex():Int return _hx_getIndex();
   inline public function getTag():String return _hx_getTag();
   inline public function getParamCount():Int return _hx_getParamCount();
   inline public function getParamI(inIndex:Int):Dynamic return _hx_getParamI(inIndex);
   inline public function getParameters():Array<Dynamic> return _hx_getParameters();
   #else
   public function __EnumParams():Array<Dynamic>;
   public function __Tag():String;
   public function __Index():Int;

   inline public function _hx_getIndex():Int  return untyped __Index();
   inline public function _hx_getTag():String  return untyped __Tag();
   inline public function _hx_getParamCount():Int return untyped __EnumParams()==null ? 0 : __EnumParams().length;
   inline public function _hx_getParamI(inIndex:Int):Dynamic return untyped __EnumParams()[inIndex];
   inline public function _hx_getParameters():Array<Dynamic> return __EnumParams()==null ? [] : __EnumParams();

   #end
}

