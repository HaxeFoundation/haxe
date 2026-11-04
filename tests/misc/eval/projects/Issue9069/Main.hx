class Main {
  static function main() {
    foo();
    foo(42);
    foo("");
  }

  @:deprecated
  static extern inline overload function foo():Void {}

  static extern inline overload function foo(i:Int):Void {}

  @:deprecated
  static extern inline overload function foo(s:String):Void {}
}
