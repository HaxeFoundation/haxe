enum E {
    A(i:Int);
    B;
}

class Main {
    static function main() {
        var e = B;

        switch (e) { // Correct: Unmatched patterns: B
            case A(10):
        }

        var v = switch (e) { // Wrong: Unmatched patterns: A(_)
            case A(10): null;
        }
    }
}
