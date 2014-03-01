namespace haxe.test
{

public class GenericHelper
{
	public Generic1<Base.InnerClass> typedGeneric;

	public static Generic1<X> staticTypedGeneric<X>(System.Type cl) where X : Base.InnerClass
	{
		return new Generic1<X>();
	}
}

}
