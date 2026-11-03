@:generic
class Foo<@:const T:Int> {
   public function new() {
       trace(T);
   }
}

class Main {
    public static function main() {
    	new Foo<10>();
		new Foo<"10">();
    }
}