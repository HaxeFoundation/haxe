namespace haxe.test
{

public class Generic1<T> where T : Base
{
	public T value;

	public Generic1()
	{

	}

	public Generic1(T value)
	{
		this.value = value;
	}

	public void setValue(T v)
	{
		this.value = v;
	}

	public T getValue()
	{
		return value;
	}

	public string getSomeString()
	{
		return value.someString;
	}

	public string complexTypeParameterOfTypeParameter<B2>(B2 b) where B2 : T
	{
		return b.someString + value.someString;
	}
}

}
