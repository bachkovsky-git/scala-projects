package func.modular.recipe

import func.modular.recipe.browser.Browser
import func.modular.recipe.simple.SimpleDatabase
import func.modular.recipe.student.StudentDatabase

object Runner {
  def main(args: Array[String]): Unit = {
    val db = if (args(0) == "student") StudentDatabase else SimpleDatabase
    val browser = new Browser(db)

    for {
      apple <- db.foodNamed("Apple")
      recipe <- browser.recipeUsing(apple)
    } println(recipe)

    for (elem <- db.allCategories) {
      browser.displayCategory(elem)
    }
  }
}
