package p2;

class C2 {
    static function main() {
        var err = Sys.stderr();
        err.writeString(A.some);
        err.writeString(getSome());
        err.writeString(Std.string(1.triple()));
    }
}
