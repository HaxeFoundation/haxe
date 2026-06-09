class Test {
    static function f(?i:Int, fn:Array<Dynamic>-> Void){
    }

    static function main() {
        // this compiles
        f(1, function(res){
            for (a in res){}
        });
        // this used to fail with `You can't iterate on a Dynamic value, please specify Iterator or Iterable`
        f(function(res){
            for (a in res){}
        });
    }
}
