import cs.system.collections.generic.List_1;

typedef T = {id:String};

class Main {
	public static function main() {
		var list:List_1<T> = new List_1();
		list.ConvertAll((t:T) -> t.id);
	}
}