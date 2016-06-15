namespace haxe.test
{

public class StaticAndInstanceClash : Base.InnerClass
{
	~StaticAndInstanceClash() {}
	public static string someString;

	public class StaticAndInstanceClashSame : StaticAndInstanceClash
	{
		public int someInt;
	}

	public class StaticAndInstanceAndMethodClash : StaticAndInstanceClashSame
	{

		private float sfloat = 5;
		private int sint = 5;
		public float someFloat()
		{
			return sfloat;
		}

		public float someFloat(float val)
		{
			return sfloat = val;
		}

		public int someInt()
		{
			return sint;
		}
	}
}

}
