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
	
	public static char charTest(char c)
	{
		return c;
	}

	public static byte byteTest(byte b)
	{
		return b;
	}

	public static int throwsTest() throws java.io.IOException
	{
		return 5;
	}

	public static int throwsTest(float someArg) throws java.lang.Throwable
	{
		return (int) someArg;
	}

	public int throwsMemberTest() throws java.lang.Throwable
	{
		return 6;
	}

	public int throwsMemberTest(boolean someArg) throws java.io.IOException
	{
		return 10;
	}

	public int varNameClash;

	public int varNameClash2;

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

		public int varNameClash2()
		{
			return 1;
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
	
	public static interface VarNameClash
	{
		int varNameClash2();
		double varNameClash2(int i);
	}

	public static class InnerClass2 extends InnerClass implements OverloadInterface1, OverloadInterface2, VarNameClash
	{
		public void someOverloadedMethod(String a1)
		{

		}

		public int someOverloadedMethod(int a1)
		{
			return a1;
		}

		public double varNameClash2(int i)
		{
			return i * 1.1;
		}
	}

	public static class _InnerClass3_
	{
		public static class InnerClass4_
		{
		}
	}
}
