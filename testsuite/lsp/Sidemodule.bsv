typedef Bit#(32) TestType;
typedef struct { Bit#(32) foo; } Bar deriving (Eq);
interface MyInterface;
    method Action foo();
    method Bit#(32) bar(TestType baz);
endinterface

interface BigFoo;
    interface MyInterface sub;
    method Bool lol;
endinterface