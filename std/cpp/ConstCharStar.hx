package cpp;

abstract ConstCharStar( RawConstPointer<Char> ) to(RawConstPointer<Char>)
{
   inline function new(s:String) this = untyped s.__s;

   @:from
   static public inline function fromString(s:String) return new ConstCharStar(s);

   @:to @:extern
   public inline function toString():String return new String(untyped this);

    @:to @:extern
    public inline function toPointer() return this;
}

