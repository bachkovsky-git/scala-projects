package func.modular.recipe.student

import func.modular.recipe.food.Recipe

trait StudentRecipes {
  this: StudentFoods =>

  def allRecipes: List[Recipe] = List(HeatItUp)

  object HeatItUp extends Recipe(
    "heat it up",
    List(FrozenFood),
    "Microwave the 'food for 10 minutes"
  )
}
