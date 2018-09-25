enum MyEnum {
    A(a:Int);
}

class Test
{
    public static function main()
    {
        switch(A(123)) {
            case A(0): 0;
        };
    }
}