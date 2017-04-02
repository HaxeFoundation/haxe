class Main {
  static function main() {
    return Gen.gen(Main, User);
  }
}

extern class Gen {
    public static function gen(type:Dynamic, ?attrs:Dynamic, children:haxe.extern.Rest<Dynamic>):Void;
}
