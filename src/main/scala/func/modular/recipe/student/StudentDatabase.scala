package func.modular.recipe.student

import func.modular.recipe.db.Database
import func.modular.recipe.food.FoodCategory

object StudentDatabase extends Database with StudentFoods with StudentRecipes {
  override def allCategories: List[FoodCategory] = List(FoodCategory("edible", List(FrozenFood)))
}
