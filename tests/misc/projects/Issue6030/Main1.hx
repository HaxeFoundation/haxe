class Main1 {
    public var ok(default,null):()->Void;
    public var stillOk(default,never) = (function(){return function(){trace("This works.");};})();
    public var notOk(default,never) = (function(){return function(){trace("This does not work. "+this);};})();

    public function new(){
        ok = (function(){return function(){trace("This works. "+this);};})();
    }

    static function main() {
        new Main1();
    }
}