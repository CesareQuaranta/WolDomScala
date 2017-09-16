package scala.wol.dom

class Vector3f(val x: Float = 0.0f,val y: Float = 0.0f, val z:Float = 0.0f) extends Cloneable{
  def +(v: Vector3f) = new Vector3f(this.x+v.x,this.y+v.y,this.z+v.z)
  def -(v: Vector3f) = new Vector3f(this.x-v.x,this.y-v.y,this.z-v.z)
  def *(v: Vector3f) = new Vector3f(this.x*v.x,this.y*v.y,this.z*v.z)
  def /(v: Vector3f) = new Vector3f(this.x/v.x,this.y/v.y,this.z/v.z)

  def negate = new Vector3f(this.x * -1,this.y * -1,this.z * -1)

  def abs = new Vector3f(Math.abs(this.x),Math.abs(this.y),Math.abs(this.z))

  def scale(s :Float) = new Vector3f(this.x * s,this.y * s,this.z * s)
  def scale(s :Double) = new Vector3f((this.x * s).toFloat,(this.y * s).toFloat,(this.z * s).toFloat)

  def pow(p :Float)= new Vector3f(Math.pow(this.x, p).toFloat,Math.pow(this.y, p).toFloat,Math.pow(this.z, p).toFloat)
  def pow(p :Double)= new Vector3f(Math.pow(this.x, p).toFloat,Math.pow(this.y, p).toFloat,Math.pow(this.z, p).toFloat)

  def sqrt= new Vector3f(Math.sqrt(this.x).toFloat,Math.sqrt(this.y).toFloat,Math.sqrt(this.z).toFloat)

  /**
    * Linearly interpolates between this vector and vector v1 and
    * return the result :  i = (1-alpha)*this + alpha*t1.
    *
    * @param v1    the first vector
    * @param alpha the alpha interpolation parameter
    */
  def interpolate(v1: Vector3f, alpha: Float) = new Vector3f((1 - alpha) * this.x + alpha * v1.x,(1 - alpha) * this.y + alpha * v1.y, (1 - alpha) * this.z + alpha * v1.z)

  /**
    * Return the cross product of dis vector and vector v1.
    *
    * @param v1 the other vector
    * @return new Vector3f result of cross product of this vector and vector v1
    *
    */
  def cross(v1: Vector3f) = new Vector3f(this.y * v1.z - this.z * v1.y, v1.x * this.z - v1.z * this.x, this.x * v1.y - this.y * v1.x)

  /**
    * Computes the dot product of this vector and vector v1.
    *
    * @param v1 the other vector
    * @return Float : the dot product of this vector and v1
    */
  def dot(v1: Vector3f) = this.x * v1.x + this.y * v1.y + this.z * v1.z

  /**
    * Return this vector normalized.
    */
  def normalize = {
    val norm = (1.0 / Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z))
    new Vector3f((this.x * norm).toFloat,(this.y * norm).toFloat,(this.z * norm).toFloat)
  }
  /**
    * Computes the distance between this point and point v1.
    * @param v the other point
    * @return the distance
    */
  def dist(v:Vector3f) = Math.sqrt(Math.pow(this.x-v.x,2) + Math.pow(this.y-v.y,2) + Math.pow(this.z-v.z,2))

  /**
    * Computes the square of the distance between this point and
    * point v1.
    *
    * @param v1 the other vector
    * @return the square of the distance
    */
  def distSquared(v1: Vector3f) = Math.pow(this.x - v1.x,2) + Math.pow(this.y - v1.y,2) + Math.pow(this.z - v1.z,2)

  /**
    * Computes the L-1 (Manhattan) distance between this point and
    * point v1.  The L-1 distance is equal to:
    * abs(x1-x2) + abs(y1-y2) + abs(z1-z2).
    *
    * @param v1 the other point
    * @return the L-1 distance
    */
  def distL1(v1: Vector3f)= Math.abs(this.x - v1.x) + Math.abs(this.y - v1.y) + Math.abs(this.z - v1.z)


  /**
    * Computes the L-infinite distance between this point and
    * point p1.  The L-infinite distance is equal to
    * MAX[abs(x1-x2), abs(y1-y2), abs(z1-z2)].
    *
    * @param v1 the other point
    * @return the L-infinite distance
    */
  def distanceLinf(v1: Vector3f)= {
    var tmp = Math.max(Math.abs(this.x - v1.x), Math.abs(this.y - v1.y))
    Math.max(tmp, Math.abs(this.z - v1.z))
  }

  /**
    * Returns the length of this vector.
    * @return the length of this vector
    */
  def length = Math.sqrt(this.x*this.x + this.y*this.y + this.z*this.z)

  /**
    * Returns the squared length of this vector.
    *
    * @return the squared length of this vector
    */
  def lengthSquared = this.x * this.x + this.y * this.y + this.z * this.z

  /**
    * Returns the angle in radians between this vector and the vector
    * parameter; the return value is constrained to the range [0,PI].
    *
    * @param v1 the other vector
    * @return the angle in radians in the range [0,PI]
    */
  def angle(v1: Vector3f)= {
    var vDot = this.dot(v1) / (this.length * v1.length)
    if (vDot < -1.0) vDot = -1.0
    if (vDot > 1.0) vDot = 1.0
    Math.acos(vDot)
  }

  /**
    * Returns true if the L-infinite distance between this tuple
    * and tuple t1 is less than or equal to the epsilon parameter,
    * otherwise returns false.  The L-infinite
    * distance is equal to MAX[abs(x1-x2), abs(y1-y2), abs(z1-z2)].
    *
    * @param t1      the tuple to be compared to this tuple
    * @param epsilon the threshold value
    * @return true or false
    */
  def epsilonEquals(t1: Vector3f, epsilon: Float): Boolean = {
    var diff = x - t1.x
    if (diff == Float.NaN) return false
    if ((if (diff < 0) -diff
    else diff) > epsilon) return false
    diff = y - t1.y
    if (diff == Float.NaN) return false
    if ((if (diff < 0) -diff
    else diff) > epsilon) return false
    diff = z - t1.z
    if (diff == Float.NaN) return false
    if ((if (diff < 0) -diff
    else diff) > epsilon) return false
    true
  }

  override def equals(that: Any): Boolean =
    that match {
      case that: Vector3f => that.isInstanceOf[Vector3f] && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = prime  + x.hashCode();
    result = prime * result + y.hashCode();
    result = prime * result + z.hashCode();
    return result
  }

  def isEmpty =(x == 0) && (y == 0) && (z == 0)

  override def toString = "(x:"+x+";y:"+y+";z:"+z+")"

  override def clone = new Vector3f(this.x, this.y, this.z)

}
