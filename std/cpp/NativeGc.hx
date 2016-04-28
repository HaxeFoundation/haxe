package cpp;

extern class NativeGc
{
   @:native("__hxcpp_gc_mem_info")
   static public function memInfo(inWhatInfo:Int) : Float return 0.0;

   @:native("_hx_allocate_extended") @:templatedCall
   static public function allocateExtended<T>(cls:Class<T>, size:Int) : T return null;

}

