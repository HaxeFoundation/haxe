public class NoPackage
{
	public NoPackInner b;
	public bool isWorking;

	public NoPackage()
	{
		isWorking = true;
		b = new NoPackInner();
	}

	public class NoPackInner
	{
		public bool isReallyWorking;

		public NoPackInner()
		{
			isReallyWorking = true;
		}
	}
}
