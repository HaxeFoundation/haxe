package unit.issues;

#if !macro
class Issue7905 extends unit.Test {
	function test() {
    eq(new I7905_Huh().test, 42);
	}
}
#end

#if !macro
typedef I7905_Huh = I7905_GenericBuild<Int>;
@:genericBuild(unit.issues.Issue7905.I7905_GenericBuild.build())
#end
class I7905_GenericBuild<T> {
  #if macro
  static var counter = 0;
  static function build() {
    var name = 'Cls${counter++}';
    var ret = macro class $name {
      public var test:Int;
      public function new() {this.test = 42;}
    }
    haxe.macro.Context.defineType(ret);
    return haxe.macro.Context.getType(name);
  }
  #end
}