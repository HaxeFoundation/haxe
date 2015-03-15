package unit.issues;

class Issue3822 extends Test
{
	public function test()
	{
		var arr = [2,1,0];
		arr.sort(function(_,_) return 1);
		arr.sort(function(_,_) return -1);
		arr.sort(Reflect.compare);
		aeq(arr,[0,1,2]);

		arr = [2,1,0,4,6,65,4,3,6,7,8,2];
		arr.sort(function(_,_) return 1);
		arr.sort(function(_,_) return -1);
		arr.sort(Reflect.compare);
		aeq(arr, [0,1,2,2,3,4,4,6,6,7,8,65]);

		arr = [2,1,0,4,6,10,3,6,7,8,2];
		arr.sort(function(_,_) return 1);
		arr.sort(function(_,_) return -1);
		arr.sort(Reflect.compare);
		aeq(arr, [0,1,2,2,3,4,6,6,7,8,10]);
	}
}
