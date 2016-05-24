typedef A     =  { var pos: Int;           var len: Int; };
typedef NOT_A =  { var pos: Array<String>; var len: Array<Float>; };

typedef X = { @:optional var x:Int; @:optional var y:Int; };
typedef Y = { var x:Int; var y:Int; };
typedef Z = { @:optional var pos:Array<String>; @:optional var len:Array<Float>; };

class Main {

    static var a:A = { pos : 1, len : 2 };

    static function main(){

        var not_a:NOT_A = (((a:X):Y):Z); //   !!! should obviously not compile
        not_a.pos.push("");

    }
}