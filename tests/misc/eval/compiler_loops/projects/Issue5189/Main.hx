typedef TT_A = {
    a : Int,
    b : String,
    c : Array<TT_A>
}

typedef TT_B = {
    a : Int,
    b : String,
    c : Array<TT_B>,
    d : Float
}
class Main {

    static var tt_a : TT_A = { a:1, b:"", c: []};

    static function main(){
        var ttb:TT_B = tt_a;
    }
}