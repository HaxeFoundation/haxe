package cpp.vm;

// TODO: implement this
typedef Breakpoint = Dynamic;
typedef StackFrame = Dynamic;

class Debugger
{
   public static function setHandler(inHandler:Void->Void)
   {
      untyped __global__.__hxcpp_dbg_set_handler(inHandler);
   }

   // Generate a handler callback ASAP
   public static function breakASAP(?inThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_break_asap(inThread);
   }

   // Stepping
   public static function stepOver(?inThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_step_over(inThread);
   }

   public static function stepInto(?inThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_step_into(inThread);
   }

   public static function stepOut(?inThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_step_out(inThread);
   }

   // Breakpoint
   public static function addBreakpoint(inBreakpoint:Breakpoint)
   {
      return untyped __global__.__hxcpp_breakpoints_add(inBreakpoint);
   }

   public static function getBreakpoints() : Array<Breakpoint>
   {
      return untyped __global__.__hxcpp_dbg_breakpoints_get();
   }

   public static function deleteBreakpoint(inI:Int)
   {
      untyped __global__.__hxcpp_dbg_breakpoints_delete(inI);
   }

   // Thread - todo
   // public static function suspendAll()

   // Callstack
   public static function getStackFrames() : Array<StackFrame>
   {
      return untyped __global__.__hxcpp_dbg_stack_frames_get();
   }

   public static function getFiles() : Array<String>
   {
      return untyped __global__.__hxcpp_dbg_get_files();
   }

   public static function getClasses() : Array<Class<Dynamic> >
   {
      return untyped __global__.__hxcpp_dbg_get_classes();
   }
}

