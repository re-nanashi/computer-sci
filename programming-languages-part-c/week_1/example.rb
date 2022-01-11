# A longer example

class MyRational

  def initialize(num,den=1)
    if den == 0
      raise "MyRational received an inapproriate argument"
    elsif den < 0
      @num = - num
      @den = - den
    else 
      @num = num
      @den = den
    end
    reduce() # i.e., self.reduce() but private
  end

  def to_s
    ans = @num.to_s
    if @den != 1
      ans += "/"
      ans += @den.to_s
    end
    ans
  end

  def to_s2
    dens = ""
    dens = "/" + @den.to_s if @den != 1 
    @num.to_s + dens
  end

  def to_s3
    "#{@num}#{if @den == 1 then "" else "/" + @den.to_s end}"
  end

  def add! r
    a = r.num
    b = r.den
    c = @num
    d = @den
    @num = (a * d) + (b * c)
    @den = b * d
    reduce
    self
  end

  # a functional addition, so we can write r1 + r2 to
  # make a new rational
  # and built-in syntactic sugar will work: can write r1 + r2
  def + r
    ans = MyRational.new(@num,@den)
    ans.add! r
    #returning ans not really needed because add! returns self
  end

# can be used by class instances and subclasses
protected
  def num 
    @num
  end

  def den
    @den
  end

# can only be used by the particular object
private
  def gcd(x,y) # recursive method calls work as expected
    if x == y
      x
    elsif x < y
      gcd(x,y-x)
    else
      gcd(y,x)
    end
  end

  def reduce 
    if @num == 0
      @den = 1
    else 
      d = gcd(@num.abs, @den) # notice method call on number
      @num = @num / d
      @den = @den / d
    end
  end
end

# top-level method (just part of object class) for testing
def use_rationals
  r1 = MyRational.new(3,4)
  r2 = r1 + r1 + MyRational.new(-5,2)
  puts r2.to_s
  (r2.add! r1).add! (MyRational.new(1,-4))
  puts r2.to_s
  puts r2.to_s2
  puts r2.to_s3
end
