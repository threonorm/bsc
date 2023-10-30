import Vector::*;
import Sidemodule::*;

module mkMyInterface(MyInterface);
    method Action foo();
        noAction;
    endmethod

    method Bit#(32) bar(TestType baz);
        return 0;
    endmethod
endmodule

module mkBigInterface(BigFoo);
    let x <- mkMyInterface;

    interface MyInterface sub = x;

    method Bool lol(); 
        return True;
    endmethod
endmodule