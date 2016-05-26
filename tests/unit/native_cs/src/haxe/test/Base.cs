namespace haxe.test
{

public class Base
{
	~Base() { someString = null; }
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

	public string prop 
	{
		get
		{
			return "SomeValue";
		}
	}

	public int this[int i]
	{
		get { return i * 20; }
	}

	public int Issue4325 { get; protected set; }

	public void setIssue4325(int val)
	{
		this.Issue4325 = val;
	}

	public int this[int i, int j]
	{
		get { return i * j; }
	}

	public int optional(int i=42)
	{
		return i * 10;
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
		~InnerClass() { privateField = 0; }

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

// Issue #3474

public interface ITextFile
{
	string Property { get; }
}

public interface ITextBuffer : ITextFile
{
}

public interface IEditableTextFile : ITextFile
{
	new string Property { get; set; }
}

public interface IEditableTextBuffer : IEditableTextFile, ITextBuffer
{
}

public class lowerCaseClass
{
	public bool works;
	public lowerCaseClass()
	{
		works = true;
	}
}

}
