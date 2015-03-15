package cpp;

abstract CastCharStar( RawPointer<Char> ) to(RawPointer<Char>)
{
   inline function new(s:String) this = untyped s.__s;

   @:from
   static public inline function fromString(s:String) return new CastCharStar(s);

    @:to
    public inline function toPointer() return this;
}

