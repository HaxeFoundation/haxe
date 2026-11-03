class C {
    static function main() {
        var err = Sys.stderr();
        err.writeString(A.some);
        err.writeString(getSome());
    }
}
