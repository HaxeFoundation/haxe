class Main {
  static function main() {
    foo({strings: []});
  }

  static function foo(data:{strings:Array<String>}) {
    var ref:References<Main> = data.strings;
    trace(ref);
  }
}

@:forward
abstract References<M:Main>(Array<Int>) from Array<Int> {
    @:from
    public static inline function fromStrings<M:Main>(v:Array<String>):References<M>
        return [for(s in v) Std.parseInt(s)];
}
