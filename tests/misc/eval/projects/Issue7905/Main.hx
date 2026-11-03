#if !macro
typedef Huh = GenericBuild<Int>;
@:genericBuild(Main.GenericBuild.build())
#end
class GenericBuild<T> {
  #if macro
  static var counter = 0;
  static function build() {
    var name = 'Cls${counter++}';
    var ret = macro class $name {
      public function new() {}
    }
    haxe.macro.Context.defineType(ret);
    return haxe.macro.Context.getType(name);
  }
  #end
}

class Main {
  static function main() {
    #if !macro
    $type(Huh.new);
    #end
  }
}