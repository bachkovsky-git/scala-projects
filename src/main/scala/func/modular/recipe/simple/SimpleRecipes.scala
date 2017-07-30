package func.modular.recipe.simple

import func.modular.recipe.food.Recipe

trait SimpleRecipes {
  this: SimpleFoods =>

  object FruitSalad extends Recipe(
    "fruit salad",
    List(Apple, Orange, Cream, Sugar),
    "Stir it all together"
  )

  def allRecipes: List[Recipe] = List(FruitSalad)
}
