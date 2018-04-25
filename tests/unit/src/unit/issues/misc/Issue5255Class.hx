package unit.issues.misc;

typedef Issue5255Class<T> = Issue5255Class_<T>;

class Issue5255Class_<T> {
	@:generic public static function test<T>(arg:T){
		return arg;
	}
}
