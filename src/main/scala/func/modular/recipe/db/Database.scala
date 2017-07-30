package func.modular.recipe.db

import func.modular.recipe.food.{Food, Foods, Recipe}

abstract class Database extends Foods {
  def allFoods: List[Food]

  def allRecipes: List[Recipe]

  def foodNamed(name: String): Option[Food] =
    allFoods.find(_.name == name)
}
