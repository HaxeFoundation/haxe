package haxe.test;

public class GenericHelper
{
	//untyped Generic - equivalent to Generic1<Dynamic>
	public Generic1 untypedGeneric;

	public Generic1 getUntypedGeneric()
	{
		return untypedGeneric;
	}

	public Generic1<Base.InnerClass> typedGeneric;

	public static <X extends Base.InnerClass> Generic1<X> staticTypedGeneric(Class<X> cl)
	{
		return new Generic1<X>();
	}
}
