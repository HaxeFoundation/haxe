/*
 * Copyright (C)2005-2017 Haxe Foundation
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

class Gc
{
   public static inline var MEM_INFO_USAGE = 0;
   public static inline var MEM_INFO_RESERVED = 1;
   public static inline var MEM_INFO_CURRENT = 2;
   public static inline var MEM_INFO_LARGE = 3;


   // Introduced hxcpp_api_level 310
   // Returns stats on memory usage:
   //   MEM_INFO_USAGE - estimate of how much is needed by program (at last collect)
   //   MEM_INFO_RESERVED - memory allocated for possible use
   //   MEM_INFO_CURRENT - memory in use, includes uncollected garbage.
   //     This will generally saw-tooth between USAGE and RESERVED
   //   MEM_INFO_LARGE - Size of separate pool used for large allocs.  Included in all the above.
   static public function memInfo(inWhatInfo:Int) : Int
   {
      return Std.int(NativeGc.memInfo(inWhatInfo));
   }

   // Returns Float
   static public function memInfo64(inWhatInfo:Int) : Float
   {
      return NativeGc.memInfo(inWhatInfo);
   }

   static public function memUsage() : Int
   {
      return Std.int(NativeGc.memInfo(MEM_INFO_USAGE));
   }

   static public function versionCheck() { return true; }



   @:native("__hxcpp_enable") @:extern
   static public function enable(inEnable:Bool) : Void { }

   @:native("__hxcpp_collect") @:extern
   static public function run(major:Bool) : Void { }

   @:native("__hxcpp_gc_compact") @:extern
   static public function compact() : Void { }

   @:native("__hxcpp_gc_trace") @:extern
   static public function nativeTrace(sought:Class<Dynamic>,printInstances:Bool) : Int return 0;

   static public function trace(sought:Class<Dynamic>,printInstances:Bool=true) : Int
   {
      return nativeTrace(sought,printInstances);
   }

   @:native("__hxcpp_gc_do_not_kill") @:extern
   static public function doNotKill(inObject:Dynamic) : Void { }

   @:native("__hxcpp_get_next_zombie") @:extern
   static public function getNextZombie() : Dynamic return null;

   @:native("__hxcpp_gc_safe_point") @:extern
   static public function safePoint() : Void { }

   @:native("__hxcpp_enter_gc_free_zone") @:extern
   static public function enterGCFreeZone() : Void { }

   @:native("__hxcpp_exit_gc_free_zone") @:extern
   static public function exitGCFreeZone() : Void { }

   @:native("__hxcpp_set_minimum_free_space") @:extern
   static public function setMinimumFreeSpace(inBytes:Int) : Void { }

   @:native("__hxcpp_set_target_free_space_percentage") @:extern
   static public function setTargetFreeSpacePercentage(inPercentage:Int) : Void { }

   @:native("__hxcpp_set_minimum_working_memory") @:extern
   static public function setMinimumWorkingMemory(inBytes:Int) : Void { }

   @:native("__hxcpp_set_finalizer") @:extern
   static public function setFinalizer<T>(inObject:T, inFinalizer:cpp.Callable<T->Void> ) : Void { }
}

