package unit.issues;

abstract Issue5192_Test(Int) {
  @:generic public static function doSomething<T>():T {
    return cast null;
  }
}
