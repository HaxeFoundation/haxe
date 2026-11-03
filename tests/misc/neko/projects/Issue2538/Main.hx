import haxe.ds.Option;

class Main {
  static function main(){
        function foo():Bool { return true; }
        map(function(num:Int):Void {
            if(foo()) 10;   //VerifyError: Error #1030: Stack depth is unbalanced. 2 != 1.
            //if(true) 10;    //works
        });
  }
  inline //removing inline fixes.
  public static function map<T,K>(f:T->K):Void {
        switch (None) {
        case Some(t): Some(f(t));
        case None: None;
        }
    }
}