/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package cpp.vm;

import haxe.Stack;

class Debugger
{
   public static inline var BRK_THIS      = -2;
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

   public static function setThread(?inDebugThread:Thread)
   {
      untyped __global__.__hxcpp_dbg_set_thread(inDebugThread==null?Thread.current().handle:inDebugThread.handle);
   }


   // Generate a handler callback ASAP
   public static function setBreak(inMode:Int)
   {
      untyped __global__.__hxcpp_dbg_set_break(inMode);
   }

   public static function exit()
   {
      untyped __global__.__hxcpp_dbg_set_break(BRK_TERMINATE);
   }
   public static function breakBad()
   {
      untyped __global__.__hxcpp_dbg_set_break(BRK_THIS);
   }



   // Breakpoint
   public static function addBreakpoint(inFileId:Int, inLine:Int)
   {
      untyped __global__.__hxcpp_breakpoints_add(inFileId, inLine);
   }

   public static function getBreakpoints() : Array<String>
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
   public static function getStackFrames() : Array<haxe.StackItem>
   {
      return untyped __global__.__hxcpp_dbg_stack_frames_get();
   }

   public static function getStackVars(inFrame:Int) : Array<String>
   {
      return untyped __global__.__hxcpp_dbg_get_stack_vars(inFrame);
   }

   public static function getStackVar(inFrame:Int,inVar:String) : Dynamic
   {
      return untyped __global__.__hxcpp_dbg_get_stack_var(inFrame,inVar);
   }

   public static function setStackVar(inFrame:Int,inVar:String, inValue:Dynamic)
   {
      untyped __global__.__hxcpp_dbg_set_stack_var(inFrame,inVar,inValue);
   }

   public static function getFiles() : Array<String>
   {
      return untyped __global__.__hxcpp_dbg_get_files();
   }
}

