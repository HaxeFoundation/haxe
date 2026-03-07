abstract Future<T>(Dynamic) {
  @:from static inline function ofAny<T>(v:T):Future<T>
    return null;
}

typedef FutureAlias<T> = Future<T>;

class Main {
  static function main() req([1, '{"baz":"baz"}']);
  static public function req(?arr, ?body:Source) {}
}

abstract Source(Int) from Int to Int {
  @:from static function ofFuture(f:FutureAlias<Source>):Source
    return null;
}
