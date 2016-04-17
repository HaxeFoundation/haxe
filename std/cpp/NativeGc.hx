package cpp;

extern class NativeGc
{
   @:native("__hxcpp_gc_mem_info")
   static public function memInfo(inWhatInfo:Int) : Float return 0.0;
}

