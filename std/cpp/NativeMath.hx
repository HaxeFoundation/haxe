package cpp;

@:noPackageRestrict
extern class NativeMath
{
#if (cpp && !cppia)
   @:native("_hx_idiv")
   public static function idiv(num:Int,denom:Int):Int;
   @:native("_hx_imod")
   public static function imod(num:Int,denom:Int):Int;
   @:native("_hx_cast_int")
   public static function castInt(f:Float):Int;
   @:native("_hx_fast_floor")
   public static function fastInt(f:Float):Int;

#else

   public static inline function imod(num:Int,denom:Int):Int
      return num%denom;

   public static inline function idiv(num:Int,denom:Int):Int
      return Std.int(num/denom);

   public static inline function castInt(f:Float):Int
      return Std.int(f);

   public static inline function fastInt(f:Float):Int
      return Std.int(f);

#end
}
