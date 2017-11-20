package unit.issues;
import haxe.ds.ArraySort;

typedef IComparer<T> = T->T->Int;
class Issue6722 extends Test {
	var comparer:IComparer<Int> = Issue6722.compare;
	public function new(){
		super();
	}
	static public function compare(a:Int, b:Int):Int {
		var i:Int = a-b;
		if (i > 0) return 1;
		else if (i < 0) return -1;
		else return 0;
	}
	function test(){
		var arr = [0,3,1];
		ArraySort.sort(arr, this.comparer);
		eq(arr + '', "[0,1,3]");
	}

}

