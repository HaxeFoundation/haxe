class Test {
    static function main() {
        var struct:Struct = {url: 'www.example.com', method: 'GET'};
        var promise:Generic<Struct> = new Generic({url: struct, method: 'GET'});

    }
}

class Generic<T> {
  public function new(v:T) {}
}

typedef Struct = {
    url:String,
    method:String,
}