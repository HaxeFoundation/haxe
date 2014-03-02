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

	virtual public int SomeProp
	{
		get { return 42; }
	}

	virtual public int SomeProp2
	{
		get { return 42; }
	}

}

public delegate void VoidVoid();

}
