package func.modular.recipe.browser

import func.modular.recipe.db.Database
import func.modular.recipe.food.{Food, FoodCategory, Recipe}

class Browser(private val database: Database) {
  def displayCategory(category: FoodCategory): Unit = println(category)

  def recipeUsing(food: Food): List[Recipe] =
    database.allRecipes.filter(_.ingredients.contains(food))
}
