var v1:E1 = A(1);
var v2 = A("foo");
var v3:E1 = B;
var v4 = B;

Type.getEnum(v1) == E1;
Type.getEnum(v2) == E2;
Type.getEnum(v3) == E1;
Type.getEnum(v4) == E2;
