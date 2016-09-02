namespace haxe.test
{

public class MyClass
{
	virtual public void normalOverload(string a)
	{

	}

	virtual public void normalOverload(int a)
	{

	}

	virtual public void normalOverload(bool a)
	{

	}

	virtual public void normalOverload(object a)
	{

	}

	virtual public void normalOverload(long a)
	{

	}

	virtual public void normalOverload(float a)
	{

	}

	virtual public void normalOverload(double a)
	{

	}

	virtual public void normalOverload(Base a)
	{
		
	}

	virtual public void normalOverload(VoidVoid a)
	{
	}

	public void outTest(out int i)
	{
		i = 42;
	}

	public void refTest(ref int i)
	{
		i *= 42;
	}

	public void dispatch()
	{
		if (voidvoid != null)
			this.voidvoid.Invoke();
	}

	virtual public int SomeProp
	{
		get { return 42; }
	}

	virtual public int SomeProp2
	{
		get { return 42; }
	}

	public event VoidVoid voidvoid;
	public static event VoidVoid voidvoid2;

	public static void dispatch2()
	{
		if (voidvoid2 != null)
			voidvoid2.Invoke();
	}

	public readonly int readonlyField = 5;
}

public delegate void VoidVoid();

}
