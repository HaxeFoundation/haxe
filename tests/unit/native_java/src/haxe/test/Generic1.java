package haxe.test;

public class Generic1<T extends Base>
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

	public String getSomeString()
	{
		return value.someString;
	}

	public <B2 extends T> String complexTypeParameterOfTypeParameter(B2 b)
	{
		return b.someString + value.someString;
	}
}
