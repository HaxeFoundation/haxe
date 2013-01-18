package unit;

abstract MyAbstract(Int) {

    public inline function new(x) {
        this = x;
    }

    public inline function incr() {
        return ++this;
    }

    public inline function toInt() : Int {
        return this;
    }

}

