package cpp.vm;

class ExecutionTrace
{
   public static function traceOff()
   {
      untyped __hxcpp_execution_trace(0);
   }
   public static function traceFunctions()
   {
      untyped __hxcpp_execution_trace(1);
   }
   public static function traceLines()
   {
      untyped __hxcpp_execution_trace(2);
   }
}
