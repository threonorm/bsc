import Vector::*;
import Sidemodule::*;


module mkBigInterface(BigFoo);
    let x <- mkMyInterface;

    interface MyInterface sub = x;

    method Bool lol(); 
        return (x.bar(?) == 0);
    endmethod
endmodule