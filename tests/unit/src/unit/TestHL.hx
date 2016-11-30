package unit;

class TestHL extends Test {

	//private function refTest(i:hl.types.Ref<Int>):Void
	//{
		//i *= 2;
	//}

	private function refTestAssign(i:hl.Ref<Int>):Void
	{
		i.set(2);
	}

	public function testRef()
	{
		var i = 10;
		refTestAssign(i);
		eq(i, 2);

		//var i = 10;
		//refTest(i);
		//eq(i, 20);
	}
}