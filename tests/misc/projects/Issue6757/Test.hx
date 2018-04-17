import haxe.ds.Option;

class Test {
  static function which(v:Int) return
    v > 1 ? Some("abc") : Some(true);

  static function main() {
    var r = which(1);
    $type(r); // Option<Dynamic>
    trace(r);
  }
}
