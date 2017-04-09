val d = 1.2 + 3.5
val s = d.toString
val i = 3 - 1
val rem = 11 % 4
math.IEEEremainder(11.0, 4.0)
List(1, 2, 3) == List(1, 2, 3)
List(3, 2, 1) eq List(3, 2, 1)


val function = (x: Int) => x + 1
function.andThen(_ - 2)
        .andThen(_ * 3)
        .apply(10)
