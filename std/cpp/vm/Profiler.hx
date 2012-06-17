package cpp.vm;

class Profiler
{

   static public function start(?inDumpFile:String) : Void
   {
      untyped __global__.__hxcpp_start_profiler(inDumpFile);
   }

   static public function stop() : Void
   {
      untyped __global__.__hxcpp_stop_profiler();
   }

}


