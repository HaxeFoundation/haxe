package cpp;

@:include("stdlib.h")
extern class Stdlib
{
   @:native("malloc")
   public static function nativeMalloc(bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("calloc")
   public static function nativeCalloc(bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("realloc")
   public static function nativeRealloc(inPtr:cpp.RawPointer<cpp.Void>,bytes:Int) : cpp.RawPointer<cpp.Void> return null;
   @:native("free")
   public static function nativeFree(ptr:cpp.RawPointer<cpp.Void>) : Void { }
   @:native("memcpy")
   public static function nativeMemcpy(dest:cpp.RawPointer<cpp.Void>, src:cpp.RawConstPointer<cpp.Void>, bytes:Int) : Void { }

   @:native("hx::ClassSizeOf") @:templatedCall
   public static function sizeof<T>(t:T) : Int { }

   inline public static function memcpy<DEST,SRC>(dest:cpp.Pointer<DEST>, src:cpp.ConstPointer<SRC>, bytes:Int) : Void
      nativeMemcpy(cast dest.ptr, cast src.ptr, bytes);

   inline public static function malloc<T>(bytes:Int) : cpp.Pointer<T>
      return cast nativeMalloc(bytes);

   inline public static function calloc<T>(bytes:Int) : cpp.Pointer<T>
      return cast nativeCalloc(bytes);

   inline public static function realloc<T>(ioPtr:cpp.Pointer<T>, bytes:Int) : Void
      ioPtr.setRaw( nativeRealloc(cast ioPtr.ptr, bytes) );

   inline public static function free<T>(ptr:cpp.Pointer<T>) : Void
   {
      if (ptr!=null)
      {
         nativeFree(cast ptr.ptr);
         ptr.ptr = null;
      }
   }
}
