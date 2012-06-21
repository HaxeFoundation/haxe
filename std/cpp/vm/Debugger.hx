package cpp.vm;

// TODO: implement this
typedef Breakpoint = Dynamic;
typedef StackFrame = Dynamic;

class Debugger
{
   public static inline var BRK_TERMINATE = -1;

   public static inline var BRK_NONE  = 0;
   public static inline var BRK_ASAP  = 1;
   public static inline var BRK_STEP  = 2;
   public static inline var BRK_ENTER = 3;
   public static inline var BRK_LEAVE = 4;

   public static function setHandler(inHandler:Void->Void)
   {
      untyped __global__.__hxcpp_dbg_set_handler(inHandler);
   }

   // Generate a handler callback ASAP
   public static function setBreak(inMode:Int,?inIgnoreThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_set_break(inMode,inIgnoreThread==null?null:inIgnoreThread.handle);
   }

   public static function exit()
   {
      untyped __global__.__hxcpp_dbg_set_break(BRK_TERMINATE,null);
   }

   // Breakpoint
   public static function addBreakpoint(inBreakpoint:Breakpoint)
   {
      untyped __global__.__hxcpp_breakpoints_add(inBreakpoint);
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

   public static function getStackVars(inFrame:Int) : Array<String>
   {
      return untyped __global__.__hxcpp_dbg_get_stack_vars(inFrame);
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

