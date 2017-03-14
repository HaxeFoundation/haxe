package cpp;

extern class NativeGc
{
   @:native("__hxcpp_gc_mem_info")
   static public function memInfo(inWhatInfo:Int) : Float return 0.0;

   @:native("_hx_allocate_extended") @:templatedCall
   static public function allocateExtended<T>(cls:Class<T>, size:Int) : T return null;

   @:native("_hx_add_finalizable")
   public static function addFinalizable( instance:{ function finalize():Void; }, inPin:Bool ): Void {}

   @:native("hx::InternalNew")
   public static function allocGcBytesRaw( inBytes : Int, isContainer:Bool ): RawPointer<cpp.Void> return null;

   inline public static function allocGcBytes( inBytes : Int ): Pointer<cpp.Void>
   {
      return Pointer.fromRaw( allocGcBytesRaw(inBytes, false) );
   }


   @:native("__hxcpp_enable") @:extern
   static public function enable(inEnable:Bool) : Void { }

   @:native("__hxcpp_collect") @:extern
   static public function run(major:Bool) : Void { }

   @:native("__hxcpp_gc_compact") @:extern
   static public function compact() : Void { }

   @:native("__hxcpp_gc_trace") @:extern
   static public function nativeTrace(sought:Class<Dynamic>,printInstances:Bool) : Int return 0;

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
}

