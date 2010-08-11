package cpp.vm;

class Gc
{
   static public function enable(inEnable:Bool) : Void
   {
      untyped __global__.__hxcpp_enable(inEnable);
   }

   static public function run(major:Bool) : Void
   {
      untyped __global__.__hxcpp_collect();
   }

   static public function trace(sought:Class<Dynamic>) : Void
   {
      untyped __global__.__hxcpp_gc_trace(sought);
   }

}
