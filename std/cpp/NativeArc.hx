package cpp;

extern class NativeArc
{
   @:native("(__bridge_transfer id)")
   public static function _bridgeTransfer<T>(ptr:cpp.RawPointer<cpp.Void> ):cpp.RawPointer<T>;

   public static inline function bridgeTransfer<T>(ptr:cpp.RawPointer<cpp.Void> ):T return cast _bridgeTransfer(ptr);
}



