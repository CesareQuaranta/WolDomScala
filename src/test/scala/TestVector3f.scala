
import org.scalatest.FunSuite

import scala.wol.dom.Vector3f;

class TestVector3f extends FunSuite{

  test("Vector3f.Equals") {
    var v01 = new Vector3f();
    var v02 = new Vector3f();
    assert( v01 == v01)
    assert( v01 == v02)

    var vo = new Vector3f( x = 3, y = 2,z = 4);
    var vc = vo.clone()
    assert( vo == vc)
    assert( v01 != vo && vc != v02)

  }
  test("Vector3f.Length") {
    var vx = new Vector3f(1,0,0);
    var vy = new Vector3f(0,1,0);
    var vz = new Vector3f(0,0,1);
    assert(vx.length === vy.length && vy.length === vz.length && vz.length === 1.0);

    var sq = Math.sqrt(1.0/3.0).toFloat;
    var v1 = new Vector3f(sq,sq,sq);
    assert(v1.length - 1.0 < 1.0E-12);

    var v4xy = new Vector3f(2,2,0);
    var v4yz = new Vector3f(0,2,2);
    var v4xz = new Vector3f(2,0,2);
    assert(v4xy.length === v4yz.length && v4yz.length === v4xz.length)

  }

  test("Vector3f Sum") {
    var v0 = new Vector3f(0,0,0);
    var v1 = new Vector3f(1,1,1);
    var vs1 = v0 + v1;
    var vs2 = v1 + v0;
    assert(vs1.x === 1.0 && vs1.y === 1.0 && vs1.z === 1.0)
    assert(v0.length === 0.0 && v1 == vs1 && vs1 == vs2 && vs2 == v1)
  }

  test("Vector3f Subtraction") {
    var v0 = new Vector3f(0,0,0);
    var v1 = new Vector3f(1,1,1);
    var v2 = new Vector3f(2,2,2);
    var vs11 = v1 - v1;
    var vs22 = v2 - v2;
    var vs21 = v2 - v1;
    var vs12 = v1 - v2;
    var vs12n = vs12.negate //(a-b = -(b-a)
    assert(vs11 == vs22 && vs11 == v0 && vs22 == v0)
    assert(vs21 != vs12)
    assert(vs21 == v1)
    //assert(vs21 == vs12n)

  }
}
