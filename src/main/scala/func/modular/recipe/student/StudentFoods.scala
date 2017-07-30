package func.modular.recipe.student

import func.modular.recipe.food.{Food, Foods}

trait StudentFoods extends Foods {
  override def allFoods: List[Food] = List(FrozenFood)

  object FrozenFood extends Food("FrozenFood")
}
