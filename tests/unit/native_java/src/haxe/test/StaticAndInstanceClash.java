package haxe.test;

public class StaticAndInstanceClash extends Base.InnerClass
{
	public static String someString;

	public static class StaticAndInstanceClashSame extends StaticAndInstanceClash
	{
		public int someInt;
		//thankfully, Java forbids this
		//public static int someInt;
	}

	public static class StaticAndInstanceAndMethodClash extends StaticAndInstanceClashSame
	{
		//thankfully, Java forbids this
		//public static float someFloat;

		//Haxe will ignore this declaration, since @:overload is only meant for methods, not for methods + member variables
		public float someFloat;

		public float someFloat()
		{
			return someFloat;
		}

		public float someFloat(float val)
		{
			return someFloat = val;
		}

		public int someInt()
		{
			return someInt;
		}
	}
}
