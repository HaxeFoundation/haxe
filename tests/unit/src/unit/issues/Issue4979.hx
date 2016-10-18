package unit.issues;

class Issue4979 extends Test {
  function test() {
    var vec = new haxe.ds.Vector<Int>(10);
    var arr = run(vec.toArray());
    eq(arr.length, 10);
  }

  static public function run(items:Array<Int>) {
    return items;
  }
}
