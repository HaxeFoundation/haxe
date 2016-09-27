package cpp;

@:scalar @:coreType
extern abstract AtomicInt from(Int) to(Int)
{
   // returns true if exchange took place
   @:native("_hx_atomic_exchange_if")
   public static function exchangeIf(ioValue:Pointer<AtomicInt>, test:Int, newVal:Int) : Bool;

   // returns value before increment
   @:native("_hx_atomic_inc")
   public static function atomicInc(ioValue:Pointer<AtomicInt>) : Int;

   // returns value before decrement
   @:native("_hx_atomic_dec")
   public static function atomicDec(ioValue:Pointer<AtomicInt>) : Int;
}

