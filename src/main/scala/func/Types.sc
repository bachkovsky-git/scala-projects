class Food

abstract class Animal {
  type SuitableFood <: Food

  def eat(food: SuitableFood): Unit = {}
}

class Grass extends Food

class Cow extends Animal {
  override type SuitableFood = Grass
}

class Horse extends Animal {
  override type SuitableFood = Grass
}

class DogFood extends Food

class Dog extends Animal {
  override type SuitableFood = DogFood
}

val bobs = new Dog()
bobs.eat(new DogFood)
//bobs.eat(new Grass) -- nope

val cow = new Cow()
cow.eat(new Grass)

val horse = new Horse()
horse.eat(new Grass)

//structural subtyping - refinement types
val animalsEatingGrass: List[Animal { type SuitableFood = Grass }] = cow :: new Cow() :: Nil //ok
//val animalsEatingGrass: List[Animal { type SuitableFood = Grass }] = cow :: horse :: Nil   //nope

