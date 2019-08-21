class Main {
  public static function main():Void {
    function foo(?a:haxe.ds.Option<Int>->Bool, b:Int):Void {}
    foo(
      o -> o.match(haxe.ds.Option.Some(_ => "3")),
      1
    );
  }
}