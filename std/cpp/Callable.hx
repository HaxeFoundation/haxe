package cpp;

#if cpp

typedef Callable<T> = Function<T, cpp.abi.Abi >

#else

@:noPackageRestrict
abstract Callable<T>(T)
{
   public var call(get,never):T;

   inline public function new(inValue:T) this = inValue;

   inline function get_call():T return this;
}


#end


