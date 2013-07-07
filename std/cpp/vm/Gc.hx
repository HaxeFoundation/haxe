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

   static public function compact() : Void
   {
      untyped __global__.__hxcpp_gc_compact();
   }

   static public function memUsage() : Int
   {
      return untyped __global__.__hxcpp_gc_used_bytes();
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
