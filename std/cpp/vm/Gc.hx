package cpp.vm;

class Gc
{
   static public function enable(inEnable:Bool) : Void
   {
      untyped __global__.__hxcpp_enable(inEnable);
   }

   static public function run(major:Bool) : Void
   {
      untyped __global__.__hxcpp_collect(major);
   }

   static public function trace(sought:Class<Dynamic>,printInstances:Bool=true) : Int
   {
      return untyped __global__.__hxcpp_gc_trace(sought,printInstances);
   }

   static public function versionCheck() { return true; }

   static public function doNotKill(inObject:Dynamic) : Void
   {
      untyped __global__.__hxcpp_gc_do_not_kill(inObject);
   }

   static public function getNextZombie() : Dynamic
   {
      return untyped __global__.__hxcpp_get_next_zombie();
   }

   static public function safePoint() : Void
   {
      untyped __global__.__hxcpp_gc_safe_point();
   }

   static public function enterGCFreeZone() : Void
   {
      untyped __global__.__hxcpp_enter_gc_free_zone();
   }

   static public function exitGCFreeZone() : Void
   {
      untyped __global__.__hxcpp_exit_gc_free_zone();
   }
}
