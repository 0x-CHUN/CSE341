class Point
    attr_accessor :x, :y
    def initialize(x,y)
        @x = x
        @y = y
    end
    def distFromOrigin # direct field access
        Math.sqrt(@x*@x + @y*@y)
    end
    def distFromOrigin2 # use getters
        Math.sqrt(x*x + y*y)
    end
end

class ColorPoint < Point
    attr_accessor :color
    def initialize(x,y,c)
        super(x,y)
        @color = c
    end
end

p = Point.new(0,0)
cp = ColorPoint.new(0,0,"red")
