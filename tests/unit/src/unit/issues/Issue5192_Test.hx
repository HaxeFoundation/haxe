package unit.issues;

class Issue5192_Test extends Test {
}

abstract Issue5192_Test_Abstract(Int) {
  @:generic public static function doSomething<T>():T {
    return cast null;
  }
}
