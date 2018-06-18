package unit.issues;

class Issue6635 extends Test
{
  public function test()
  {
    var t = Main.main();
  }
}

@:keep private abstract A<V>(String) {}

private typedef S = {a:A<Int>};

@:keep private class Main {
	function new(s:S) {}

	static function getS():S return null;

	public static function main() {
		return new Main(getS());
	}
}