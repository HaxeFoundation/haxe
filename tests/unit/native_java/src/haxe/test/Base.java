package haxe.test;

public class Base
{
	//some haxe-specific keywords

	public static final int inline = 42;
	public static final int callback = 43;
	public static final int cast = 44;
	public static final int untyped = 45;

	//test haxe keyword
	public static int in = 46;

	//final + static variable = inline var in Haxe
	public static final int inlineNumber = 42;

	//cannot be inline
	public static int notInlineNumber = 42;

	public String someString;
	private String privateField;
	protected int protectedField;

	//static + nonstatic clash
	public static int nameClash(Base t)
	{
		return -1;
	}

	public int nameClash()
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

	public int varNameClash;

	public static class InnerClass extends Base
	{
		private int privateField = 42;

		//protected override without explicit override tag
		protected int protectedFunction()
		{
			return privateField;
		}

		public int nameClash()
		{
			return 10;
		}

		public static int getValue(OverloadInterface2 oiface)
		{
			return oiface.someOverloadedMethod(42);
		}

		public static class InnerInnerClass extends InnerClass2
		{


			//protected override without explicit override tag
			protected int protectedFunction()
			{
				return 10;
			}
		}
	}

	public static class InnerClass2 extends InnerClass implements OverloadInterface1, OverloadInterface2
	{
		public void someOverloadedMethod(String a1)
		{

		}

		public int someOverloadedMethod(int a1)
		{
			return a1;
		}
	}
}
