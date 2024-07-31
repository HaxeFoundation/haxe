// Should be a warning here, UNLESS Macro.printStuff() calls reportError()/error()
@:build(Macro.buildMain())
class Main {
  static function main() {
    final x = 5 - ""; // No errors shown for this, completion/hover does not work
  }
}