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
}

