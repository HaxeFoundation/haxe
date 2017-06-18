package unit.issues;

class Issue5751 extends Test {
#if (java || cs)
  public function test() {
#if java
    var arr = @:privateAccess (new Array(new java.NativeArray(10)) : Array<Null<Int>>);
#elseif cs
    var arr = @:privateAccess (new Array(new cs.NativeArray(10)) : Array<Null<Int>>);
#end
    eq(arr[0], null);
    arr[0] = 100;
    eq(arr[0], 100);
  }
#end
}
