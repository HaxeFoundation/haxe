class Main{
	public static function main(){}
}

typedef Recursive<T1> = {
	public function rec<T2>(a:Recursive<T1>):Recursive<T2>;
}

class RecClass<T1> {
	public function rec<T2>(a:Recursive<T1>):Recursive<T2> return a;
}