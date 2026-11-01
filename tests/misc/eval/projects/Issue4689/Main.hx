class Main
{
    static function main()
    {
        test(Gadt.B);
    }

    private static function test<T>(a:Gadt<T>)
    {
        var x = switch (a)
        {
            case A:
                "test";
        }
    }
}

enum Gadt<T> {
    A:Gadt<String>;
    B:Gadt<Float>;
}