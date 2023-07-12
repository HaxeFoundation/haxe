package unit.issues;

class Issue4286<Const> extends Test
{
	public function test()
	{
		t(true);
		var s:Something<10> = null;
	}
}

interface Something<Const>
{
}
