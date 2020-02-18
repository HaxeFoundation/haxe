class Main {
	static function f(?i:Int, fn:Array<Dynamic>->Void){}

	static function main() {
		f(function(res){
			$type(res);
			for (a in res){}
		});
	}
} 