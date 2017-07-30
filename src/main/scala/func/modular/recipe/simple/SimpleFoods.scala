package func.modular.recipe.simple

import func.modular.recipe.food.{Food, FoodCategory, Foods}

trait SimpleFoods extends Foods {

  def allFoods = List(Apple, Pear)

  def allCategories = List(
    FoodCategory("fruits", List(Pear, Apple, Orange)),
    FoodCategory("misc", List(Sugar, Cream))
  )

  object Pear extends Food("Pear")

  object Apple extends Food("Apple")

  object Orange extends Food("Orange")

  object Sugar extends Food("Sugar")

  object Cream extends Food("Cream")

}

