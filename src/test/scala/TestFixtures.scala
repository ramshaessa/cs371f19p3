package edu.luc.cs.laufer.cs473.expressions

object TestFixtures {

  import ast._

  val complex1string = "((1+2)-(3*4))/5;"

  val complex1 =
    Block(
      Div(
        Minus(
          Plus(
            Constant(1),
            Constant(2)
          ),
          Times(
            Constant(3),
            Constant(4)
          )
        ),
        Constant(5)
      )
    );

  val complex2string = "((1+2)-(3*4))%5;"

  val complex2 =
    Block(
      Mod(
        Minus(
          Plus(
            Constant(1),
            Constant(2)
          ),
          Times(
            Constant(3),
            Constant(4)
          )
        ),
        Constant(5)
      )
    );

  val assignment1string = "x=5;"

  val assignment1 =
    Block(
      Assignment(
        Variable("x"),
        Constant(5)
      )
    );

  val assignment2string = "x=5; y=7;"

  val assignment2 =
    Block(
      Assignment(
        Variable("x"),
        Constant(5)
      ),
      Assignment(
        Variable("y"),
        Constant(7)
      )
    );

  val complex3string = "x = ((1 + y2) - (3 * y4)) / 5;"

  val complex3 =
    Block(
      Assignment(
        Variable("x"),
        Div(
          Minus(
            Plus(
              Constant(1),
              Variable("y2")
            ),
            Times(
              Constant(3),
              Variable("y4")
            )
          ),
          Constant(5)
        )
      )
    );

  val conditional1string = "if (1) { x = 2; }"

  val conditional1 =
    Block(
      Conditional(
        Constant(1),
        Block(
          Assignment(
            Variable("x"),
            Constant(2)
          )
        ),
        Block()
      )
    );

  val conditional2string = "if (1) { x = 2; } else { x = 3; }"

  val conditional2 =
    Block(
      Conditional(
        Constant(1),
        Block(
          Assignment(
            Variable("x"),
            Constant(2)
          )
        ),
        Block(
          Assignment(
            Variable("x"),
            Constant(3)
          )
        )
      )
    );

  val block1string = "{ r = r + x; y = y + 1; }"

  val block1 =
    Block(
      Block(
        Assignment(
          Variable("r"),
          Plus(
            Variable("r"),
            Variable("x")
          )
        ),
        Assignment(
          Variable("y"),
          Plus(
            Variable("y"),
            Constant(1)
          )
        )
      )
    );

  val loop1string = "while (y) { r = r + x; y = y - 1; }"

  val loop1 =
    Block(
      Loop(
        Variable("y"),
        Block(
          Assignment(
            Variable("r"),
            Plus(
              Variable("r"),
              Variable("x")
            )
          ),
          Assignment(
            Variable("y"),
            Minus(
              Variable("y"),
              Constant(1)
            )
          )
        )
      )
    );

  val loop2string = "x = 2; y = 3; r = 0; while (y) { r = r + x ; y = y - 1; }"

  val loop2 =
    Block(
      Assignment(
        Variable("x"),
        Constant(2)
      ),
      Assignment(
        Variable("y"),
        Constant(3)
      ),
      Assignment(
        Variable("r"),
        Constant(0)
      ),
      Loop(
        Variable("y"),
        Block(
          Assignment(
            Variable("r"),
            Plus(
              Variable("r"),
              Variable("x")
            )
          ),
          Assignment(
            Variable("y"),
            Minus(
              Variable("y"),
              Constant(1)
            )
          )
        )
      )
    );

}