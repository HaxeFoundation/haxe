class Main {
  static function main() {
    trace("Haxe is great!");
    #if js
      trace('x');
    #elsif js
      trace('elif');
    #else
      trace('end');
    #end
  }
}