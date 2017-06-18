package unit.issues;

class Issue5751 extends Test {
#if cs
  public function test() {
    var arr = @:privateAccess (new Array(new cs.NativeArray(10)) : Array<Null<Int>>);
    eq(arr[0], null);
    arr[0] = 100;
    eq(arr[0], 100);
  }
#end
}
