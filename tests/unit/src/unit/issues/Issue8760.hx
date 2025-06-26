package unit.issues;

using unit.issues.Issue8760.SortStringTools;
using unit.issues.Issue8760.SortFloatTools;

private class SortStringTools {
	public static function sorted<T:String, A:Array<T>>(arr:A) {
		return 'sorted<T:String>()';
	}
  }

private class SortFloatTools {
	public static function sorted<T:Float, A:Array<T>>(arr:A) {
		return 'sorted<T:Float>()';
	}
  }

class Issue8760 extends Test {
	function test() {
		var arr1 = ["abc", "def"];
        eq("sorted<T:String>()", arr1.sorted());

        var arr2 = [123,456];
        eq("sorted<T:Float>()", arr2.sorted());
	}
}