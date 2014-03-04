namespace haxe.test
{

public class Base
{
	//some haxe-specific keywords

	public static readonly int inline = 42;
	public static readonly int callback = 43;
	public static readonly int cast = 44;
	public static int untyped = 45;

	//final + static variable = inline var in Haxe
	const int inlineNumber = 42;

	//cannot be inline
	public static int notInlineNumber = 42;

	public string someString;
	private string privateField;
	protected int protectedField;

	//static + nonstatic clash
	public static int nameClash(Base t)
	{
		return -1;
	}

	public virtual int nameClash()
	{
		return 1;
	}

	protected int protectedFunction()
	{
		return protectedField;
	}

	public int varNameClash(int b)
	{
		return b;
	}

	public static double varNameClash(double d)
	{
		return d;
	}
	
	public static char charTest(char c)
	{
		return c;
	}

	public static byte byteTest(byte b)
	{
		return b;
	}

	public class InnerClass : Base
	{
		private int privateField = 42;

		//protected override without explicit override tag
		protected int protectedFunction()
		{
			return privateField;
		}

		public override int nameClash()
		{
			return 10;
		}

		public static int getValue(OverloadInterface2 oiface)
		{
			return oiface.someOverloadedMethod(42);
		}

		public class InnerInnerClass : InnerClass2
		{


			//protected override without explicit override tag
			protected int protectedFunction()
			{
				return 10;
			}
		}
	}

	public class InnerClass2 : InnerClass, OverloadInterface1, OverloadInterface2
	{
		public void someOverloadedMethod(string a1)
		{

		}

		public int someOverloadedMethod(int a1)
		{
			return a1;
		}
	}
}

}
