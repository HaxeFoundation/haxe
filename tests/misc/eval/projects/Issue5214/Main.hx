class C<T:{a:Int}>{
    var v:T;
    public function new(v:T) this.v = v;
}

class Main {

    static var ab:{a:Int,b:String} = {a:1,b:""};
    static var c  = new C(ab);
    static var c1 = new C({a:1,b:""});

    static function main(){
        var c  = new C(ab);
        var c1 = new C({a:1,b:""});
    }
}
